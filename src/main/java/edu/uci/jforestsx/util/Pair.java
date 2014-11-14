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

package edu.uci.jforestsx.util;

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class Pair<A, B> {

	public A fst;
	public B snd;

	public Pair(A fst, B snd) {
		this.fst = fst;
		this.snd = snd;
	}

	public A getFirst() {
		return fst;
	}

	public B getSecond() {
		return snd;
	}

	public void setFirst(A v) {
		fst = v;
	}

	public void setSecond(B v) {
		snd = v;
	}

	@Override
	public String toString() {
		return "Pair[" + fst + "," + snd + "]";
	}

	private static boolean equals(Object x, Object y) {
		return (x == null && y == null) || (x != null && x.equals(y));
	}

	@Override
	@SuppressWarnings("rawtypes")
	public boolean equals(Object other) {
		return other instanceof Pair && equals(fst, ((Pair) other).fst)
				&& equals(snd, ((Pair) other).snd);
	}

	@Override
	public int hashCode() {
		if (fst == null)
			return (snd == null) ? 0 : snd.hashCode() + 1;
		else if (snd == null)
			return fst.hashCode() + 2;
		else
			return fst.hashCode() * 17 + snd.hashCode();
	}

	public static <A, B> Pair<A, B> of(A a, B b) {
		return new Pair<A, B>(a, b);
	}
}
