package org.maltparser.core.pool;


import org.maltparser.core.exception.MaltChainedException;
import org.maltparser.core.helper.HashSet;


public abstract class ObjectPoolSet<T> extends ObjectPool<T> {
	private final HashSet<T> available;
	private final HashSet<T> inuse;
	
	public ObjectPoolSet() {
		this(Integer.MAX_VALUE);
	}
	
	public ObjectPoolSet(int keepThreshold) {
		super(keepThreshold);
		available = new HashSet<T>();
		inuse = new HashSet<T>();
	}
	
	protected abstract T create() throws MaltChainedException;
	public abstract void resetObject(T o) throws MaltChainedException;
	
	public synchronized T checkOut() throws MaltChainedException {
		if (available.isEmpty()) {
			T t = create();
			inuse.add(t);
			return t;
		} else {
			for (T t : available) {
				inuse.add(t);
				available.remove(t);
				return t;
			}
		}
		return null;
	}
	
	public synchronized void checkIn(T t) throws MaltChainedException {
		resetObject(t);
		inuse.remove(t);
		if (available.size() < keepThreshold) {
			available.add(t);
		}
	}
	
	public synchronized void checkInAll() throws MaltChainedException {
		for (T t : inuse) {
			resetObject(t);
			if (available.size() < keepThreshold) {
				available.add(t);
			}
		}
		inuse.clear();
	}
	
	public String toString() {
		final StringBuilder sb = new StringBuilder();
		for (T t : inuse) {
			sb.append(t);
			sb.append(", ");
		}
		return sb.toString();
	}
}
