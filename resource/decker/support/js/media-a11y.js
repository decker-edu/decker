/**
 * Common module for a11y and handout mode to apply modifications to videos.
 */

/**
 * Removes autoplay and adds controls to a video if not already done so.
 * Raises the lock to the modifications by 1 if already done.
 * @param {*} media
 */
export function modifyMedia(media) {
  if (!media.dataset.originalAutoplay) {
    media.dataset.originalAutoplay = media.dataset.autoplay;
  }
  if (!media.dataset.originalControls) {
    media.dataset.originalControls = media.controls ? "true" : "false";
  }
  const modifiedCount = media.dataset.modifiedCount
    ? Number(media.dataset.modifiedCount) + 1
    : 1;
  media.dataset.modifiedCount = modifiedCount;
  // Pause the video if it is playing. A blind call to video.pause() may cause an error because the
  // Promise of video.play() that Reveal caused has not resolved yet.
  if (media.currentTime !== 0 && !media.paused) {
    media.pause();
  }
  media.setAttribute("controls", "");
  delete media.dataset.autoplay;
}

/**
 * Reduces the lock to a video's modifications by 1.
 * If the count would go from 1 to 0, remove the modifications.
 * @param {*} media
 */
export function restoreMedia(media) {
  const modifiedCount = media.dataset.modifiedCount
    ? Number(media.dataset.modifiedCount)
    : 0;
  if (modifiedCount <= 0) {
    console.error(
      "[media-a11y] Restoring an unmodified video. This should not happen."
    );
  } else if (modifiedCount === 1) {
    if (!(media.dataset.originalAutoplay === "undefined")) {
      media.dataset.autoplay = media.dataset.originalAutoplay;
    }
    if (media.dataset.originalControls === "true") {
      media.setAttribute("controls", "");
    } else {
      media.removeAttribute("controls");
    }
    delete media.dataset.modifiedCount;
  } else {
    media.dataset.modifiedCount = modifiedCount - 1;
  }
}
