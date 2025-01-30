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

import net.codecadenza.eclipse.model.domain.DomainAttribute;

/**
 * <p>
 * Container for domain attributes that are used in the reverse engineering process
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class RevEngDomainAttribute {
	private final boolean createdByReverseEngineering;
	private final DomainAttribute domainAttribute;
	private final RevEngDomainObject parentObject;

	/**
	 * Constructor
	 * @param parentObject
	 * @param domainAttribute
	 * @param createdByReverseEngineering
	 */
	public RevEngDomainAttribute(RevEngDomainObject parentObject, DomainAttribute domainAttribute,
			boolean createdByReverseEngineering) {
		this.parentObject = parentObject;
		this.domainAttribute = domainAttribute;
		this.createdByReverseEngineering = createdByReverseEngineering;
	}

	/**
	 * @return the domain object the attribute belongs to
	 */
	public RevEngDomainObject getParentObject() {
		return parentObject;
	}

	/**
	 * @return true if the attribute has been created by the reverse engineering process
	 */
	public boolean isCreatedByReverseEngineering() {
		return createdByReverseEngineering;
	}

	/**
	 * @return the domain attribute
	 */
	public DomainAttribute getDomainAttribute() {
		return domainAttribute;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((domainAttribute == null) ? 0 : domainAttribute.getName().hashCode());
		result = prime * result + ((parentObject == null) ? 0 : parentObject.hashCode());

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

		final var other = (RevEngDomainAttribute) obj;

		if (parentObject == null) {
			if (other.parentObject != null)
				return false;
		}
		else if (!parentObject.equals(other.parentObject))
			return false;

		if (domainAttribute == null) {
			if (other.domainAttribute != null)
				return false;
		}
		else if (!domainAttribute.getName().equals(other.domainAttribute.getName()))
			return false;

		return true;
	}

}
