#!/bin/bash

# Get the mode from the event info, or fall back to querying aerospace
if [ ! -z "$INFO" ]; then
    MODE="$INFO"
else
    MODE=$(aerospace list-modes --current)
    if [ -z "$MODE" ]; then
        MODE="unknown"
    fi
fi

# Update the SketchyBar item
sketchybar --set aerospace_mode label="$MODE"
