package org.thunlp.thulac.io;

/**
 * An interface used to listen to the starting and termination events of the
 * segmentation program.
 */
public interface IProgramStateListener {
	/**
	 * Called when the segmentation program starts.
	 */
	void onProgramStart();

	/**
	 * Called when the segmentation program terminates. (in finally block)
	 */
	void onProgramEnd();
}
