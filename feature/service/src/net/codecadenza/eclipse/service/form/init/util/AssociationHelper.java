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
package net.codecadenza.eclipse.service.form.init.util;

import java.util.Set;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import org.eclipse.emf.common.util.BasicEList;
import org.eclipse.emf.common.util.EList;

/**
 * <p>
 * Helper class for creating valid association aliases and join fragments for JPA queries
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class AssociationHelper {
	public static final String INITIAL_ALIAS = "a";

	// At a dedicated position an alias contains one character between 'a' and 'z'!
	private static final int NUMBER_OF_CHARACTERS = 26;

	// The first character is 'a'
	private static final int OFFSET = 97;

	private static final String RESERVED_ALIAS = "x";

	private String alias;
	private int counter;
	private boolean outerJoin;
	private final String name;
	private final AbstractDomainAssociation assoc;
	private EList<AssociationHelper> children = new BasicEList<>();
	private AssociationHelper parent;
	private boolean loaded;

	/**
	 * Constructor
	 * @param alias
	 * @param counter
	 * @param name
	 * @param assoc
	 */
	public AssociationHelper(String alias, int counter, String name, AbstractDomainAssociation assoc) {
		this.alias = alias;
		this.counter = counter;
		this.name = name;
		this.assoc = assoc;
	}

	/**
	 * Constructor
	 * @param alias
	 * @param counter
	 * @param name
	 * @param assoc
	 * @param parent
	 */
	public AssociationHelper(String alias, int counter, String name, AbstractDomainAssociation assoc, AssociationHelper parent) {
		this(alias, counter, name, assoc);

		this.parent = parent;
	}

	/**
	 * Increment the counter and create the next alias
	 */
	public void nextAlias() {
		counter++;

		int result = counter / NUMBER_OF_CHARACTERS;

		if (result == 0) {
			result = OFFSET + counter;
			alias = String.valueOf((char) (result));

			if (alias.equals(RESERVED_ALIAS))
				nextAlias();
		}
		else {
			result = OFFSET + (result - 1);
			alias = String.valueOf((char) (result));

			final int rest = counter % NUMBER_OF_CHARACTERS + OFFSET;

			alias += String.valueOf((char) (rest));
		}
	}

	/**
	 * Search an association helper by the given domain association
	 * @param assoc
	 * @return the respective root object or null if it could not be found
	 */
	public AssociationHelper searchAssociationHelper(AbstractDomainAssociation assoc) {
		for (final AssociationHelper a : getChildren()) {
			if (a.getAssociation().equals(assoc))
				return a;

			final AssociationHelper result = searchAssociationHelper(a, assoc);

			if (result != null)
				return result;
		}

		return null;
	}

	/**
	 * Check if the alias of the given association already exits in the tree and change it accordingly!
	 */
	public void checkAlias() {
		boolean checkFullTree = false;
		final AssociationHelper rootAssoc = findRootAssociation();

		// We have to go down the tree branches until the alias is unique!
		while (true) {
			checkFullTree = false;

			// We start with the root node of this branch!
			for (final AssociationHelper assocHelper : rootAssoc.getChildren()) {
				// We must not check this association
				if (assocHelper.equals(this))
					continue;

				// If the alias already exists we have to run the test again!
				if (assocHelper.getAlias().equals(getAlias())) {
					checkFullTree = true;
					nextAlias();
					break;
				}

				if (!checkAlias(this, assocHelper)) {
					checkFullTree = true;
					break;
				}
			}

			if (!checkFullTree)
				break;
		}
	}

	/**
	 * Recursive method for creating a join statement
	 * @param aliasOfInterest
	 * @param usedJoins
	 * @param forceLeftJoin
	 * @return the generated join statement
	 */
	public String generateJoinStatement(String aliasOfInterest, Set<String> usedJoins, boolean forceLeftJoin) {
		var join = "";

		for (final AssociationHelper child : getChildren()) {
			// Build the join fragment
			if (child.isOuterJoin() || forceLeftJoin) {
				// A left outer join is necessary if either the parent association or this association is optional!
				join = " left join ";
				forceLeftJoin = true;
			}
			else
				join = " join ";

			join += child.getParent().getAlias() + "." + child.getName() + " " + child.getAlias();

			if (child.getAlias().equals(aliasOfInterest)) {
				// The alias has been found but it must be checked if this join fragment is already used!
				if (!usedJoins.contains(join))
					usedJoins.add(join);
				else
					join = "";

				return join;
			}

			// We have to go down the whole tree until we find the alias we are searching for!
			final String subJoins = child.generateJoinStatement(aliasOfInterest, usedJoins, forceLeftJoin);

			if (!subJoins.isEmpty()) {
				// It must be checked if this join fragment is already used!
				if (!usedJoins.contains(join)) {
					usedJoins.add(join);
					return join + subJoins;
				}

				// We must omit this join fragment but all sub-fragments must be added!
				return subJoins;
			}
		}

		return "";
	}

	/**
	 * @return the alias
	 */
	public String getAlias() {
		return alias;
	}

	/**
	 * @return true if the association needs an outer join
	 */
	public boolean isOuterJoin() {
		return outerJoin;
	}

	/**
	 * @param outerJoin
	 */
	public void setOuterJoin(boolean outerJoin) {
		this.outerJoin = outerJoin;
	}

	/**
	 * @return the name
	 */
	public String getName() {
		return name;
	}

	/**
	 * @return the association
	 */
	public AbstractDomainAssociation getAssociation() {
		return assoc;
	}

	/**
	 * @return the counter
	 */
	public int getCounter() {
		return counter;
	}

	/**
	 * @return a list containing all children
	 */
	public EList<AssociationHelper> getChildren() {
		return children;
	}

	/**
	 * @param children
	 */
	public void setChildren(EList<AssociationHelper> children) {
		this.children = children;
	}

	/**
	 * @return the parent of this object
	 */
	public AssociationHelper getParent() {
		return parent;
	}

	/**
	 * @param parent
	 */
	public void setParent(AssociationHelper parent) {
		this.parent = parent;
	}

	/**
	 * @return true if this object has been already loaded
	 */
	public boolean isLoaded() {
		return loaded;
	}

	/**
	 * @param loaded
	 */
	public void setLoaded(boolean loaded) {
		this.loaded = loaded;
	}

	/**
	 * @return the root association of this branch
	 */
	private AssociationHelper findRootAssociation() {
		if (parent == null)
			return this;

		AssociationHelper thisBranch = parent;

		while (true) {
			if (thisBranch.getAlias().equals(INITIAL_ALIAS))
				return thisBranch;

			thisBranch = thisBranch.parent;
		}
	}

	/**
	 * @param helper
	 * @param assoc
	 * @return the respective root object or null if it could not be found
	 */
	private AssociationHelper searchAssociationHelper(AssociationHelper helper, AbstractDomainAssociation assoc) {
		for (final AssociationHelper a : helper.getChildren()) {
			if (a.getAssociation().equals(assoc))
				return a;

			final AssociationHelper result = searchAssociationHelper(a, assoc);

			if (result != null)
				return result;
		}

		return null;
	}

	/**
	 * @param associationToCheck
	 * @param currentAssociation
	 * @return false if the selected alias already exists in the tree
	 */
	private boolean checkAlias(AssociationHelper associationToCheck, AssociationHelper currentAssociation) {
		for (final AssociationHelper assocHelper : currentAssociation.getChildren()) {
			if (associationToCheck.equals(assocHelper))
				continue;

			if (associationToCheck.getAlias().equals(assocHelper.getAlias())) {
				associationToCheck.nextAlias();
				return false;
			}

			if (!checkAlias(associationToCheck, assocHelper))
				return false;
		}

		return true;
	}

}
