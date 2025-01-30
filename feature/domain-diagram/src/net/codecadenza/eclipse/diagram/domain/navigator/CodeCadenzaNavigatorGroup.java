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
package net.codecadenza.eclipse.diagram.domain.navigator;

import java.util.Collection;
import java.util.LinkedList;

/**
 * <p>
 * Navigator group
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CodeCadenzaNavigatorGroup extends CodeCadenzaAbstractNavigatorItem {
	private final String myGroupName;
	private final String myIcon;
	private final Collection<CodeCadenzaNavigatorItem> myChildren = new LinkedList<>();

	/**
	 * @param groupName
	 * @param icon
	 * @param parent
	 */
	CodeCadenzaNavigatorGroup(String groupName, String icon, Object parent) {
		super(parent);

		myGroupName = groupName;
		myIcon = icon;
	}

	/**
	 * @return the group name
	 */
	public String getGroupName() {
		return myGroupName;
	}

	/**
	 * @return the icon
	 */
	public String getIcon() {
		return myIcon;
	}

	/**
	 * @return the children of this group
	 */
	public Object[] getChildren() {
		return myChildren.toArray();
	}

	/**
	 * @param children
	 */
	public void addChildren(Collection<CodeCadenzaNavigatorItem> children) {
		myChildren.addAll(children);
	}

	/**
	 * @param child
	 */
	public void addChild(CodeCadenzaNavigatorItem child) {
		myChildren.add(child);
	}

	/**
	 * @return true if no child exists
	 */
	public boolean isEmpty() {
		return myChildren.isEmpty();
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if (obj instanceof final CodeCadenzaNavigatorGroup anotherGroup && getGroupName().equals(anotherGroup.getGroupName()))
			return getParent().equals(anotherGroup.getParent());

		return super.equals(obj);
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		return getGroupName().hashCode();
	}

}
