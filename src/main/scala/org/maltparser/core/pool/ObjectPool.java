package org.maltparser.core.pool;

import org.maltparser.core.exception.MaltChainedException;

public abstract class ObjectPool<T> {
	protected int keepThreshold;

	public ObjectPool() {
		this(Integer.MAX_VALUE);
	}
	
	public ObjectPool(int keepThreshold) {
		setKeepThreshold(keepThreshold);
	}
	
	public int getKeepThreshold() {
		return keepThreshold;
	}

	public void setKeepThreshold(int keepThreshold) {
		this.keepThreshold = keepThreshold;
	}
	
	protected abstract T create() throws MaltChainedException;
	public abstract void resetObject(T o) throws MaltChainedException;
	public abstract T checkOut() throws MaltChainedException;
	public abstract void checkIn(T o) throws MaltChainedException;
	public abstract void checkInAll() throws MaltChainedException;
}