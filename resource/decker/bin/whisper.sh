#!/bin/bash

basedir=$1
lang=$2
mp4=$3
vtt=$4
wav=/tmp/`basename ${mp4/.mp4/.wav}`

# construct whisper command
whisper="$basedir/main --model $basedir/models/ggml-large.bin"

# if result is *-en.vtt then translate
translate=""
if [[ $vtt == *-en.vtt ]] ; then
  translate="--translate"
fi

# extract audio from video
ffmpeg -y -i $mp4 -acodec pcm_s16le -ac 1 -ar 16000 $wav

# do the hard work
$whisper --file $wav --language $lang $translate --output-vtt --output-file ${vtt/.vtt/}

# clean up
rm -f $tmp
