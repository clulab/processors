package org.thunlp.thulac.io;

import org.thunlp.thulac.Thulac;

import java.io.IOException;
import java.util.List;

/**
 * An interface used to provide input for {@link Thulac}. Implementations of this
 * interface should contain its own context, since {@link #provideInput()} does not
 * pass any kind of parameter. It is recommended that implementations read input from a
 * stream, e.g., from a file of from the console ({@code System.in}).
 */
public interface IInputProvider extends IProgramStateListener {
	/**
	 * Provide a {@link List} of {@link String} which contains the input for the
	 * segmentation program to process. By contract, the return value of this method,
	 * joined with whitespaces (U+0020) should logically represent a line from the input,
	 * though this is not compulsory. A {@code null} return value will be regarded as
	 * an EOF and the program will terminate. A {@link List} is used because it is
	 * recommended to split an enormous line into separate line segments based on the
	 * punctuations.
	 *
	 * @return The input to the segmentation program.
	 */
	List<String> provideInput() throws IOException;
}
