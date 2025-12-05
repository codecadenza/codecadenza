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
package net.codecadenza.runtime.ddt.service.data.util;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.UUID;
import net.codecadenza.runtime.ddt.model.MethodInvocation;
import net.codecadenza.runtime.ddt.service.data.ITestDataProvider;

/**
 * <p>
 * Iterator for a group of {@link MethodInvocation} objects
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class MethodInvocationGroupIterator implements Iterator<MethodInvocation> {
	private final ITestDataProvider testDataProvider;
	private final UUID originalGroupId;
	private final List<MethodInvocation> allInvocations = new ArrayList<>();
	private MethodInvocation next;
	private MethodInvocation currentInvocation;

	/**
	 * Constructor
	 * @param testDataProvider
	 */
	public MethodInvocationGroupIterator(ITestDataProvider testDataProvider) {
		this.testDataProvider = testDataProvider;

		// Pre‑fetch the first element – if it is null we are already at the end
		this.next = testDataProvider.getNextInvocation();
		this.currentInvocation = this.next;

		if (this.next == null) {
			this.originalGroupId = null;
			return;
		}

		this.originalGroupId = this.next.getGroupId();
		this.allInvocations.add(this.next);

		if (this.originalGroupId == null)
			this.next = null;
	}

	/*
	 * (non-Javadoc)
	 * @see java.util.Iterator#hasNext()
	 */
	@Override
	public boolean hasNext() {
		if (next == null) {
			// Put all invocations back so that they can be consumed again in subsequent operations!
			allInvocations.reversed().stream().forEach(testDataProvider::pushBackInvocation);
			return false;
		}

		return true;
	}

	/*
	 * (non-Javadoc)
	 * @see java.util.Iterator#next()
	 */
	@Override
	public MethodInvocation next() {
		if (next == null)
			throw new NoSuchElementException("No more method invocations in this group!");

		// Fetch the next invocation – stop when crossing the group boundary
		next = testDataProvider.getNextInvocation();

		currentInvocation = next;

		if (next != null) {
			allInvocations.add(next);

			if (!originalGroupId.equals(next.getGroupId()))
				next = null;
		}

		return currentInvocation;
	}

	/**
	 * @return the current invocation
	 */
	public MethodInvocation getCurrentInvocation() {
		return currentInvocation;
	}

}
