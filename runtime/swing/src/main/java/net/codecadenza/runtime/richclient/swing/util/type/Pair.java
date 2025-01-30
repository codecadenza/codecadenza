/*
 * This file is part of CodeCadenza, a set of tools, libraries and plug-ins
 * for modeling and creating Java-based enterprise applications.
 * For more information visit:
 *
 * https://github.com/codecadenza/
 *
 * This software is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
 */
package net.codecadenza.runtime.richclient.swing.util.type;

/**
 * <p>
 * Basic implementation of a pair
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <A> the type of the first member of the pair
 * @param <B> the type of the second member of the pair
 */
public class Pair<A, B> {
	private final A fst;
	private final B snd;

	/**
	 * Constructor
	 * @param fst
	 * @param snd
	 */
	public Pair(A fst, B snd) {
		this.fst = fst;
		this.snd = snd;
	}

	/**
	 * @return the first element
	 */
	public A first() {
		return fst;
	}

	/**
	 * @return the second element
	 */
	public B second() {
		return snd;
	}

	/**
	 * @param <A>
	 * @param <B>
	 * @param fst
	 * @param snd
	 * @return a new instance of a pair
	 */
	public static <A, B> Pair<A, B> pair(A fst, B snd) {
		return new Pair<>(fst, snd);
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return String.format("(%s, %s)", fst, snd);
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		return (fst == null ? 5381 : fst.hashCode()) ^ (snd == null ? -5381 : snd.hashCode());
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object other) {
		if (other == this)
			return true;

		if (other == null || !this.getClass().equals(other.getClass()))
			return false;

		final var that = (Pair<?, ?>) other;

		return (this.fst == that.fst || this.fst != null && this.fst.equals(that.fst))
				&& (this.fst == that.snd || this.fst != null && this.fst.equals(that.snd));
	}

}
