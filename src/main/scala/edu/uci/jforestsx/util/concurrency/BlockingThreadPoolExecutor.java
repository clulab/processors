/**
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package edu.uci.jforestsx.util.concurrency;

import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.Semaphore;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

/**
 * This is a blocking version of Java's ThreadPoolExecutor.
 * It makes sure that the number of concurrently running threads
 * does not exceed the poolSize.
 * 
 * This code is based on ideas presented at:
 * http://today.java.net/pub/a/today/2008/10/23/creating-a-notifying-blocking-thread-pool-executor.html
 */
public class BlockingThreadPoolExecutor extends ThreadPoolExecutor {

	private Semaphore semaphore;
	private AtomicInteger tasksInProcess = new AtomicInteger();
	private final Lock lock = new ReentrantLock();
	private final Condition done = lock.newCondition();

	private static BlockingThreadPoolExecutor instance;

	public synchronized static void init(int poolSize) {
		if (instance != null) {
			return;
		}
		instance = new BlockingThreadPoolExecutor(poolSize);
	}

	public static BlockingThreadPoolExecutor getInstance() {
		return instance;
	}

	private BlockingThreadPoolExecutor(int poolSize) {
		super(poolSize, poolSize, 0L, TimeUnit.MILLISECONDS, new LinkedBlockingQueue<Runnable>());
		semaphore = new Semaphore(poolSize);
	}

	@Override
	public void execute(Runnable task) {
		boolean acquired = false;
		do {
			try {
				semaphore.acquire();
				acquired = true;
			} catch (InterruptedException e) {
				// wait forever!
			}
		} while (!acquired);

		tasksInProcess.incrementAndGet();

		try {
			super.execute(task);
		} catch (RuntimeException e) {
			// specifically, handle RejectedExecutionException
			tasksInProcess.decrementAndGet();
			semaphore.release();
			throw e;
		} catch (Error e) {
			tasksInProcess.decrementAndGet();
			semaphore.release();
			throw e;
		}
	}

	@Override
	protected void afterExecute(Runnable r, Throwable t) {
		super.afterExecute(r, t);
		synchronized (this) {
			tasksInProcess.decrementAndGet();
			if (tasksInProcess.intValue() == 0) {
				lock.lock(); // MUST lock!
				try {
					done.signalAll();
				} finally {
					lock.unlock(); // unlock even in case of an exception
				}
			}
		}
		semaphore.release();
	}

	public void await() {
		try {
			lock.lock();
			try {
				while (tasksInProcess.get() > 0) { // avoid signaling on 'spuriously' wake-up
					done.await();
				}
			} finally {
				lock.unlock(); // unlock even in case of an exception
			}
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
	}
}
