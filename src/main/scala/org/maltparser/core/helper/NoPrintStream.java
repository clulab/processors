package org.maltparser.core.helper;

import java.io.OutputStream;
import java.io.PrintStream;
import java.util.Locale;
/**
*
*
* @author Johan Hall
*/
public final class NoPrintStream extends PrintStream {
	public static final OutputStream NO_OUTPUTSTREAM = NoOutputStream.DEVNULL;
    public static final PrintStream NO_PRINTSTREAM = new NoPrintStream();
    private NoPrintStream() {
    	super(NO_OUTPUTSTREAM);
	}
	@Override
	public PrintStream append(char c) {
		return super.append(c);
	}

	@Override
	public PrintStream append(CharSequence csq, int start, int end) {
		return super.append(csq, start, end);
	}

	@Override
	public PrintStream append(CharSequence csq) {
		return super.append(csq);
	}

	@Override
	public boolean checkError() {
		return super.checkError();
	}

	@Override
	public void close() {
		super.close();
	}

	@Override
	public void flush() {
		super.flush();
	}

	@Override
	public PrintStream format(Locale l, String format, Object... args) {
		return super.format(l, format, args);
	}

	@Override
	public PrintStream format(String format, Object... args) {
		return super.format(format, args);
	}

	@Override
	public void print(boolean b) {}

	@Override
	public void print(char c) {}

	@Override
	public void print(char[] s) {}

	@Override
	public void print(double d) {}

	@Override
	public void print(float f) {}

	@Override
	public void print(int i) {}

	@Override
	public void print(long l) {}

	@Override
	public void print(Object obj) {}

	@Override
	public void print(String s) {}

	@Override
	public PrintStream printf(Locale l, String format, Object... args) {
		return super.printf(l, format, args);
	}

	@Override
	public PrintStream printf(String format, Object... args) {
		return super.printf(format, args);
	}

	@Override
	public void println() {}

	@Override
	public void println(boolean x) {}

	@Override
	public void println(char x) {}

	@Override
	public void println(char[] x) {}

	@Override
	public void println(double x) {}

	@Override
	public void println(float x) {}

	@Override
	public void println(int x) {}

	@Override
	public void println(long x) {}

	@Override
	public void println(Object x) {}

	@Override
	public void println(String x) {}

	@Override
	protected void setError() {
		super.setError();
	}

	@Override
	public void write(byte[] buf, int off, int len) {}

	@Override
	public void write(int b) {}
}

