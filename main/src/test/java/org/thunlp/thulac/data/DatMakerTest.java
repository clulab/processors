package org.thunlp.thulac.data;

import org.junit.Test;
import org.thunlp.thulac.IAccessible;
import org.thunlp.thulac.TestHelper;

import java.io.IOException;
import java.util.List;

import static org.junit.Assert.assertTrue;

/**
 *
 */
public class DatMakerTest {
	@Test
	public void test() throws IOException {
		IAccessible file = TestHelper.resourceAt("dat_maker_test_1.txt");
		Dat dat = DatMaker.readFromInputStream(file.toInputStream());
		List<String> lines = file.getLines();
		for (String line : lines) assertTrue(line, dat.contains(line));
	}
}
