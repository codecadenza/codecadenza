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
package net.codecadenza.eclipse.tools.reverse.model;

import net.codecadenza.eclipse.model.java.JavaEnum;

/**
 * <p>
 * Container for Java enumerations that are used in the reverse engineering process
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class RevEngEnum {
	private final boolean createdByReverseEngineering;
	private final JavaEnum javaEnum;
	private String namespaceName;

	/**
	 * Constructor
	 * @param namespaceName
	 * @param javaEnum
	 * @param createdByReverseEngineering
	 */
	public RevEngEnum(String namespaceName, JavaEnum javaEnum, boolean createdByReverseEngineering) {
		this.namespaceName = namespaceName;
		this.createdByReverseEngineering = createdByReverseEngineering;
		this.javaEnum = javaEnum;
	}

	/**
	 * @return the name of the namespace the enumeration belongs to
	 */
	public String getNamespaceName() {
		return namespaceName;
	}

	/**
	 * @param namespaceName the namespace name to set
	 */
	public void setNamespaceName(String namespaceName) {
		this.namespaceName = namespaceName;
	}

	/**
	 * @return true if the enumeration has been created by the reverse engineering process
	 */
	public boolean isCreatedByReverseEngineering() {
		return createdByReverseEngineering;
	}

	/**
	 * @return the Java enum
	 */
	public JavaEnum getJavaEnum() {
		return javaEnum;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;

		int result = 1;
		result = prime * result + ((javaEnum == null) ? 0 : javaEnum.getName().hashCode());
		result = prime * result + ((namespaceName == null) ? 0 : namespaceName.hashCode());

		return result;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;

		if (obj == null)
			return false;

		if (getClass() != obj.getClass())
			return false;

		final var other = (RevEngEnum) obj;

		if (namespaceName == null) {
			if (other.namespaceName != null)
				return false;
		}
		else if (!namespaceName.equals(other.namespaceName))
			return false;

		if (javaEnum == null) {
			if (other.javaEnum != null)
				return false;
		}
		else if (!javaEnum.getName().equals(other.javaEnum.getName()))
			return false;

		return true;
	}

}
