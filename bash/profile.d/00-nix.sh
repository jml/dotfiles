profile_dir="$HOME/.nix-profile/etc/profile.d"

if [ -d ${profile_dir} ]; then
  for i in ${profile_dir}/*.sh; do
    if [ -r $i ]; then
      . $i
    fi
  done
  unset i
fi

unset profile_dir
