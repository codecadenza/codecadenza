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
package net.codecadenza.eclipse.generator.common;

import java.util.HashSet;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.AssociationTagEnumeration;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.DomainTagEnumeration;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.domain.OneToOneAssociation;

/**
 * <p>
 * Generator that creates code fragments needed by different generators
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CommonGenerator {
	/**
	 * Prevent instantiation
	 */
	private CommonGenerator() {

	}

	/**
	 * @param obj
	 * @param fragment
	 * @return a cascaded field accessor including the primary key field of the client domain object
	 */
	public static String getClientAccessFragment(DomainObject obj, String fragment) {
		return getClientAccessFragment(obj, fragment, false, new HashSet<>());
	}

	/**
	 * @param obj
	 * @param fragment
	 * @return a cascaded getter including the primary key field getter of the client domain object
	 */
	public static String getClientGetterFragment(DomainObject obj, String fragment) {
		return getClientAccessFragment(obj, fragment, true, new HashSet<>());
	}

	/**
	 * @param obj
	 * @param fragment
	 * @param getter
	 * @param assocSet
	 * @return the generated content
	 */
	private static String getClientAccessFragment(DomainObject obj, String fragment, boolean getter,
			HashSet<AbstractDomainAssociation> assocSet) {
		// Check if the domain object represents the client itself!
		if (obj.getTag() != null && obj.getTag() == DomainTagEnumeration.CLIENT) {
			if (getter)
				return fragment + "." + obj.getPKAttribute().getGetterName();

			return fragment + "." + obj.getPKAttribute().getName();
		}

		// Check all associations of this domain object
		for (final AbstractDomainAssociation assoc : obj.getAllAssociations())
			if (assoc.getTag() == AssociationTagEnumeration.CLIENT_REFERENCE) {
				if (getter)
					return fragment + "." + assoc.getGetterName() + "." + assoc.getTarget().getPKAttribute().getGetterName();

				return fragment + "." + assoc.getName() + "." + assoc.getTarget().getPKAttribute().getName();
			}

		// Now we check the associations of the referenced domain objects
		for (final AbstractDomainAssociation assoc : obj.getAllAssociations()) {
			// Check if this association has been already analyzed in order to avoid an infinite loop!
			if (obj.equals(assoc.getTarget()) || assocSet.contains(assoc))
				continue;

			assocSet.add(assoc);

			if (assoc instanceof final ManyToOneAssociation mto) {
				if (mto.isOptional())
					continue;
			}
			else if (assoc instanceof final OneToOneAssociation oto) {
				if (oto.isOptional())
					continue;
			}
			else
				continue;

			if (getter) {
				final String generatedFragment = getClientAccessFragment(assoc.getTarget(), fragment + "." + assoc.getGetterName(),
						getter, assocSet);

				if (generatedFragment.isEmpty())
					continue;

				return generatedFragment;
			}

			final String generatedFragment = getClientAccessFragment(assoc.getTarget(), fragment + "." + assoc.getName(), getter,
					assocSet);

			if (generatedFragment.isEmpty())
				continue;

			return generatedFragment;
		}

		return "";
	}

}
