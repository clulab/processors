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

package edu.uci.jforestsx.sample;

import edu.uci.jforestsx.eval.EvaluationMetric;
import edu.uci.jforestsx.learning.trees.Tree;

/**
 * @author Yasser Ganjisaffar <ganjisaffar at gmail dot com>
 */

public abstract class Predictions {

	protected Sample sample;
	
	public void setSample(Sample sample) {
		this.sample = sample;
	}
	
	public abstract void allocate(int maxNumValidInstances);
	
	public abstract void update(Tree tree, double weight);
	
	public abstract double evaluate(EvaluationMetric evalMetric) throws Exception;
	
	public abstract void reset();
	
}
