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
package net.codecadenza.eclipse.testing.domain;

/**
 * <p>
 * Objects of this class represent domain associations
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DomainAssociation {
	private final String name;
	private final DomainObject target;
	private final AssociationType type;
	private final boolean bidirectional;

	/**
	 * Constructor
	 * @param name
	 * @param target
	 * @param type
	 * @param bidirectional
	 */
	public DomainAssociation(String name, DomainObject target, AssociationType type, boolean bidirectional) {
		this.name = name;
		this.target = target;
		this.type = type;
		this.bidirectional = bidirectional;
	}

	/**
	 * @return the domain association name
	 */
	public String getName() {
		return name;
	}

	/**
	 * @return the association type
	 */
	public AssociationType getType() {
		return type;
	}

	/**
	 * @return the target domain object
	 */
	public DomainObject getTarget() {
		return target;
	}

	/**
	 * @return true if the association is either a many-to-many or a one-to-many association
	 */
	public boolean isToManyAssociation() {
		return type == AssociationType.MANY_TO_MANY || type == AssociationType.ONE_TO_MANY;
	}

	/**
	 * @return true if the association is bidirectional
	 */
	public boolean isBidirectional() {
		return bidirectional;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		if (isToManyAssociation())
			return "Collection<" + target.getName() + "> " + name;

		return target.getName() + " " + name;
	}

}
