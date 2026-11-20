# Layering rules for Claude Code settings.
#
# Arrays directly under "permissions" (allow, deny, ask, additionalDirectories)
# are sets: a layer adds entries and never removes one that is already there,
# so permissions granted locally survive a merge. Every other array and every
# scalar takes the later layer's value, so an edited hook replaces the old one
# instead of running beside it.

def is_set_path($p): ($p | length) == 2 and $p[0] == "permissions";

def union($a; $b):
  reduce $b[] as $x ($a; if any(.[]; . == $x) then . else . + [$x] end);

# Merge $b over $a. Keys keep $a's order, and keys only $b has go at the end,
# so merging into the live file leaves its layout alone.
def deepmerge($a; $b; $p):
  if ($a | type) == "object" and ($b | type) == "object" then
    reduce ($b | keys_unsorted[]) as $k ($a;
      .[$k] = (if has($k) then deepmerge(.[$k]; $b[$k]; $p + [$k]) else $b[$k] end))
  elif ($a | type) == "array" and ($b | type) == "array" and is_set_path($p) then
    union($a; $b)
  else
    $b
  end;

def deepmerge($a; $b): deepmerge($a; $b; []);

# Everything in $live that $layered doesn't account for. Emits one object per
# difference:
#   {kind: "local-only", path, live}           $layered doesn't set the path
#   {kind: "changed", path, live, layered}     both set it, to different values
#   {kind: "added", path, live}                an entry in a permissions set
#                                              that $layered lacks
def drift($live; $layered; $p):
  if ($live | type) == "object" and ($layered | type) == "object" then
    $live | keys_unsorted[] as $k
    | if ($layered | has($k)) then drift($live[$k]; $layered[$k]; $p + [$k])
      else {kind: "local-only", path: ($p + [$k]), live: $live[$k]}
      end
  elif ($live | type) == "array" and ($layered | type) == "array" and is_set_path($p) then
    $live[] | select(. as $x | $layered | any(.[]; . == $x) | not)
    | {kind: "added", path: $p, live: .}
  elif $live == $layered then
    empty
  else
    {kind: "changed", path: $p, live: $live, layered: $layered}
  end;

def drift($live; $layered): [drift($live; $layered; [])];
