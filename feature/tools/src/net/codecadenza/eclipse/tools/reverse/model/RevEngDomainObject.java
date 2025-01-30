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

import java.util.ArrayList;
import java.util.List;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainObject;

/**
 * <p>
 * Container for domain objects that are used in the reverse engineering process
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class RevEngDomainObject {
	private final DomainObject domainObject;
	private final boolean createdByReverseEngineering;
	private final List<RevEngDomainAttribute> attributes = new ArrayList<>();
	private final List<RevEngDomainAssociation> associations = new ArrayList<>();
	private final ReverseEngineeringModel model;
	private String namespaceName;

	/**
	 * Constructor
	 * @param model
	 * @param namespaceName
	 * @param domainObject
	 * @param createdByReverseEngineering
	 */
	public RevEngDomainObject(ReverseEngineeringModel model, String namespaceName, DomainObject domainObject,
			boolean createdByReverseEngineering) {
		this.namespaceName = namespaceName;
		this.domainObject = domainObject;
		this.createdByReverseEngineering = createdByReverseEngineering;
		this.model = model;
	}

	/**
	 * @return the reverse engineering model
	 */
	public ReverseEngineeringModel getModel() {
		return model;
	}

	/**
	 * @return the name of the namespace the domain object belongs to
	 */
	public String getNamespaceName() {
		return namespaceName;
	}

	/**
	 * @param namespaceName
	 */
	public void setNamespaceName(String namespaceName) {
		this.namespaceName = namespaceName;
	}

	/**
	 * @return the domain object
	 */
	public DomainObject getDomainObject() {
		return domainObject;
	}

	/**
	 * @return true if the domain object has been created by the reverse engineering process
	 */
	public boolean isCreatedByReverseEngineering() {
		return createdByReverseEngineering;
	}

	/**
	 * @return the attributes
	 */
	public List<RevEngDomainAttribute> getAttributes() {
		return attributes;
	}

	/**
	 * @return the associations
	 */
	public List<RevEngDomainAssociation> getAssociations() {
		return associations;
	}

	/**
	 * Add a domain association
	 * @param assoc
	 * @param createdByReverseEngineering
	 */
	public void addAssociation(AbstractDomainAssociation assoc, boolean createdByReverseEngineering) {
		associations.add(new RevEngDomainAssociation(this, assoc, createdByReverseEngineering));
	}

	/**
	 * Add a domain attribute
	 * @param attr
	 * @param createdByReverseEngineering
	 */
	public void addAttribute(DomainAttribute attr, boolean createdByReverseEngineering) {
		attributes.add(new RevEngDomainAttribute(this, attr, createdByReverseEngineering));
	}

	/**
	 * Remove the association
	 * @param assoc
	 */
	public void removeAssociation(RevEngDomainAssociation assoc) {
		associations.remove(assoc);
	}

	/**
	 * Remove the attribute
	 * @param attribute
	 */
	public void removeAttribute(RevEngDomainAttribute attribute) {
		attributes.remove(attribute);
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;

		int result = 1;
		result = prime * result + ((domainObject.getName() == null) ? 0 : domainObject.getName().hashCode());
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

		final var other = (RevEngDomainObject) obj;

		if (namespaceName == null) {
			if (other.namespaceName != null)
				return false;
		}
		else if (!namespaceName.equals(other.namespaceName))
			return false;

		if (domainObject == null) {
			if (other.domainObject != null)
				return false;
		}
		else if (!domainObject.getName().equals(other.domainObject.getName()))
			return false;

		return true;
	}

}
