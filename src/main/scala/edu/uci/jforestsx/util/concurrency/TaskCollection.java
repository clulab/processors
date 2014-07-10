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

import java.util.ArrayList;
import java.util.List;

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class TaskCollection<T extends TaskItem> {
	private List<T> tasks;

	public TaskCollection() {
		tasks = new ArrayList<T>();
	}
	
	public TaskCollection(Class<T> _c, int taskCount) {
		tasks = new ArrayList<T>(taskCount);
		try {
			for (int i = 0; i < taskCount; i++) {
				T task = _c.newInstance();
				tasks.add(task);
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	public int getSize() {
		return tasks.size();
	}
	
	public void addTask(T task) {
		tasks.add(task);
	}
	
	public T getTask(int idx) {
		return tasks.get(idx);
	}

	public void run() {
		for (T task : tasks) {
			BlockingThreadPoolExecutor.getInstance().execute(task);
		}
		BlockingThreadPoolExecutor.getInstance().await();
	}
}
