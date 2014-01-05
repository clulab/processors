package org.maltparser.core.pool;

import java.util.ArrayList;

import org.maltparser.core.exception.MaltChainedException;

public abstract class ObjectPoolList<T> extends ObjectPool<T> {
	private final ArrayList<T> objectList;
	private int currentSize;
	
	public ObjectPoolList() {
		this(Integer.MAX_VALUE);
	}
	
	public ObjectPoolList(int keepThreshold) {
		super(keepThreshold);
		objectList = new ArrayList<T>();
	}

	protected abstract T create() throws MaltChainedException;
	public abstract void resetObject(T o) throws MaltChainedException;
	
	public synchronized T checkOut() throws MaltChainedException {
		T t = null;
		if (currentSize >= objectList.size()) {
			t = create();
			objectList.add(t);
			currentSize++;	
		} else {
			t = objectList.get(currentSize);
			currentSize++;
		}
		return t;
	}
	
	public synchronized void checkIn(T o) throws MaltChainedException {
		resetObject(o);
	}
	
	public synchronized void checkInAll() throws MaltChainedException {
		for (int i = currentSize-1; i >= 0 && i < objectList.size(); i--) {
			resetObject(objectList.get(i));
			if (currentSize >= keepThreshold) {
				objectList.remove(i);
			}
		}
		currentSize = 0;
	}

	public int getCurrentSize() {
		return currentSize;
	}

	public void setCurrentSize(int currentSize) {
		this.currentSize = currentSize;
	}
	
	public int size() {
		return objectList.size();
	}
	
	public String toString() {
		final StringBuilder sb = new StringBuilder();
		for (int i = 0; i < currentSize; i++) {
			sb.append(objectList.get(i));
			sb.append(", ");
		}
		return sb.toString();
	}
}
