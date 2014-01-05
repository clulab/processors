package org.maltparser.core.propagation.spec;

/**
 * @author Johan Hall
 *
 */
public class PropagationSpec {
	public static final long serialVersionUID = 1L;
	private String from;
	private String to;
	private String _for; // for
	private String over;
	
	public PropagationSpec(String from, String to, String _for, String over) {
		setFrom(from);
		setTo(to);
		setFor(_for);
		setOver(over);
	}
	
	public String getFrom() {
		return from;
	}
	
	public void setFrom(String from) {
		this.from = from;
	}
	
	public String getTo() {
		return to;
	}
	
	public void setTo(String to) {
		this.to = to;
	}
	
	public String getFor() {
		return _for;
	}
	
	public void setFor(String _for) {
		this._for = _for;
	}
	
	public String getOver() {
		return over;
	}
	
	public void setOver(String over) {
		this.over = over;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((_for == null) ? 0 : _for.hashCode());
		result = prime * result + ((from == null) ? 0 : from.hashCode());
		result = prime * result + ((over == null) ? 0 : over.hashCode());
		result = prime * result + ((to == null) ? 0 : to.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		PropagationSpec other = (PropagationSpec) obj;
		if (_for == null) {
			if (other._for != null)
				return false;
		} else if (!_for.equals(other._for))
			return false;
		if (from == null) {
			if (other.from != null)
				return false;
		} else if (!from.equals(other.from))
			return false;
		if (over == null) {
			if (other.over != null)
				return false;
		} else if (!over.equals(other.over))
			return false;
		if (to == null) {
			if (other.to != null)
				return false;
		} else if (!to.equals(other.to))
			return false;
		return true;
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("FROM: "); sb.append(from);sb.append("\n");
		sb.append("TO  : "); sb.append(to);sb.append("\n");
		sb.append("FOR : "); sb.append(_for);sb.append("\n");
		sb.append("OVER: "); sb.append(over);sb.append("\n");
		return sb.toString();
	}
}
