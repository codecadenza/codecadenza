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

import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.ManyToManyAssociation;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.model.domain.OneToOneAssociation;

/**
 * <p>
 * Container for domain associations that are used in the reverse engineering process
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class RevEngDomainAssociation {
	private final RevEngDomainObject parentObject;
	private final AbstractDomainAssociation association;
	private final boolean createdByReverseEngineering;

	/**
	 * Constructor
	 * @param parentObject
	 * @param association
	 * @param createdByReverseEngineering
	 */
	public RevEngDomainAssociation(RevEngDomainObject parentObject, AbstractDomainAssociation association,
			boolean createdByReverseEngineering) {
		this.parentObject = parentObject;
		this.association = association;
		this.createdByReverseEngineering = createdByReverseEngineering;
	}

	/**
	 * @return the domain object the association belongs to
	 */
	public RevEngDomainObject getParentObject() {
		return parentObject;
	}

	/**
	 * @return the association
	 */
	public AbstractDomainAssociation getAssociation() {
		return association;
	}

	/**
	 * @return true if the association has been created by the reverse engineering process
	 */
	public boolean isCreatedByReverseEngineering() {
		return createdByReverseEngineering;
	}

	/**
	 * @return true if the association represents a one-to-one association
	 */
	public boolean isOneToOne() {
		return association instanceof OneToOneAssociation;
	}

	/**
	 * @return true if the association represents a one-to-many association
	 */
	public boolean isOneToMany() {
		return association instanceof OneToManyAssociation;
	}

	/**
	 * @return true if the association represents a many-to-many association
	 */
	public boolean isManyToMany() {
		return association instanceof ManyToManyAssociation;
	}

	/**
	 * @return true if the association represents a many-to-one association
	 */
	public boolean isManyToOne() {
		return association instanceof ManyToOneAssociation;
	}

	/**
	 * @return the container for the reverse one-to-one association or null if it couldn't be found
	 */
	public RevEngDomainAssociation getReverseOneToOneAssoc() {
		final var oto = (OneToOneAssociation) association;
		final ReverseEngineeringModel model = parentObject.getModel();
		final DomainObject domainObject = parentObject.getDomainObject();

		if (oto.getReverseAssociation() == null)
			return null;

		for (final RevEngDomainObject obj : model.getDomainObjects())
			for (final RevEngDomainAssociation assocObj : obj.getAssociations()) {
				if (!assocObj.isOneToOne())
					continue;

				final var reverseAssoc = (OneToOneAssociation) assocObj.getAssociation();

				if (reverseAssoc.getTarget() == null || reverseAssoc.getName() == null)
					continue;

				if (reverseAssoc.getTarget().equals(domainObject) && reverseAssoc.getName().equals(oto.getReverseAssociation().getName()))
					return assocObj;
			}

		return null;
	}

	/**
	 * @return the container for the reverse many-to-many association or null if it couldn't be found
	 */
	public RevEngDomainAssociation getReverseManyToManyAssoc() {
		final var mtm = (ManyToManyAssociation) association;
		final ReverseEngineeringModel model = parentObject.getModel();
		final DomainObject domainObject = parentObject.getDomainObject();

		if (mtm.getReverseAssociation() == null)
			return null;

		for (final RevEngDomainObject obj : model.getDomainObjects())
			for (final RevEngDomainAssociation assocObj : obj.getAssociations()) {
				if (!assocObj.isManyToMany())
					continue;

				final var reverseAssoc = (ManyToManyAssociation) assocObj.getAssociation();

				if (reverseAssoc.getTarget() == null || reverseAssoc.getName() == null)
					continue;

				if (reverseAssoc.getTarget().equals(domainObject) && reverseAssoc.getName().equals(mtm.getReverseAssociation().getName()))
					return assocObj;
			}

		return null;
	}

	/**
	 * @return the container for the reverse one-to-many association or null if it couldn't be found
	 */
	public RevEngDomainAssociation getReverseOneToManyAssoc() {
		final var mto = (ManyToOneAssociation) association;
		final ReverseEngineeringModel model = parentObject.getModel();
		final DomainObject domainObject = parentObject.getDomainObject();

		for (final RevEngDomainObject obj : model.getDomainObjects())
			for (final RevEngDomainAssociation assocObj : obj.getAssociations()) {
				if (!assocObj.isOneToMany())
					continue;

				final var reverseAssoc = (OneToManyAssociation) assocObj.getAssociation();

				if (reverseAssoc.getTarget() == null || reverseAssoc.getReverseAssociation() == null)
					continue;

				if (reverseAssoc.getTarget().equals(domainObject) && reverseAssoc.getReverseAssociation().getName().equals(mto.getName()))
					return assocObj;
			}

		return null;
	}

	/**
	 * @return the container for the reverse many-to-one association or null if it couldn't be found
	 */
	public RevEngDomainAssociation getReverseManyToOneAssoc() {
		final var otm = (OneToManyAssociation) association;
		final ReverseEngineeringModel model = parentObject.getModel();
		final DomainObject domainObject = parentObject.getDomainObject();

		if (domainObject == null || otm.getReverseAssociation() == null)
			return null;

		for (final RevEngDomainObject obj : model.getDomainObjects())
			for (final RevEngDomainAssociation assocObj : obj.getAssociations()) {
				if (!assocObj.isManyToOne())
					continue;

				final var reverseAssoc = (ManyToOneAssociation) assocObj.getAssociation();

				if (reverseAssoc.getTarget() == null || reverseAssoc.getName() == null)
					continue;

				if (reverseAssoc.getTarget().equals(domainObject) && reverseAssoc.getName().equals(otm.getReverseAssociation().getName()))
					return assocObj;
			}

		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((association == null) ? 0 : association.getName().hashCode());
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

		final var other = (RevEngDomainAssociation) obj;

		if (parentObject == null) {
			if (other.parentObject != null)
				return false;
		}
		else if (!parentObject.equals(other.parentObject))
			return false;

		if (association == null) {
			if (other.association != null)
				return false;
		}
		else if (!association.getName().equals(other.association.getName()))
			return false;

		return true;
	}

}
