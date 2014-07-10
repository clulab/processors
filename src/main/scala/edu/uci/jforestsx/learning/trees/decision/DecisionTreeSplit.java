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

package edu.uci.jforestsx.learning.trees.decision;

import edu.uci.jforestsx.learning.trees.TreeSplit;

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public class DecisionTreeSplit extends TreeSplit {

	public double[] leftTargetDist;
	public double[] rightTargetDist;
	
	public DecisionTreeSplit(int numClasses) {
		leftTargetDist = new double[numClasses];
		rightTargetDist = new double[numClasses];
	}

	@Override
	public void copy(TreeSplit other) {
		super.copy(other);
		System.arraycopy(((DecisionTreeSplit) other).leftTargetDist, 0, leftTargetDist, 0, leftTargetDist.length);
		System.arraycopy(((DecisionTreeSplit) other).rightTargetDist, 0, rightTargetDist, 0, rightTargetDist.length);
	}

}
