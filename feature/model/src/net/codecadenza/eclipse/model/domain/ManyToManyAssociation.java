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
package net.codecadenza.eclipse.model.domain;

import net.codecadenza.eclipse.model.db.DBTable;

/**
 * A representation of the model object '<em><b>Many To Many Association</b></em>'.
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.domain.ManyToManyAssociation#getTable <em>Table</em>}</li>
 * </ul>
 * </p>
 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getManyToManyAssociation()
 * @model
 * @generated
 */
public interface ManyToManyAssociation extends AbstractDomainAssociation {
	/**
	 * Return the value of the '<em><b>Table</b></em>' containment reference
	 * @return the value of the '<em>Table</em>' containment reference
	 * @see #setTable(DBTable)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getManyToManyAssociation_Table()
	 * @model containment="true"
	 * @generated
	 */
	DBTable getTable();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.ManyToManyAssociation#getTable <em>Table</em>}' containment
	 * reference
	 * @param value the new value of the '<em>Table</em>' containment reference
	 * @see #getTable()
	 * @generated
	 */
	void setTable(DBTable value);

	/**
	 * @return true if this association is bidirectional
	 * @generated not
	 */
	boolean isBidirectional();

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#getReverseAssociation()
	 * @generated not
	 */
	@Override
	ManyToManyAssociation getReverseAssociation();

}
