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

import net.codecadenza.eclipse.model.db.DBColumn;

/**
 * A representation of the model object '<em><b>One To One Association</b></em>'.
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.domain.OneToOneAssociation#isOptional <em>Optional</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.OneToOneAssociation#getColumn <em>Column</em>}</li>
 * </ul>
 * </p>
 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getOneToOneAssociation()
 * @model
 * @generated
 */
public interface OneToOneAssociation extends AbstractDomainAssociation {
	/**
	 * Return the value of the '<em><b>Optional</b></em>' attribute
	 * @return the value of the '<em>Optional</em>' attribute
	 * @see #setOptional(boolean)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getOneToOneAssociation_Optional()
	 * @model
	 * @generated
	 */
	boolean isOptional();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.OneToOneAssociation#isOptional <em>Optional</em>}' attribute
	 * @param value the new value of the '<em>Optional</em>' attribute
	 * @see #isOptional()
	 * @generated
	 */
	void setOptional(boolean value);

	/**
	 * Return the value of the '<em><b>Column</b></em>' reference
	 * @return the value of the '<em>Column</em>' reference
	 * @see #setColumn(DBColumn)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getOneToOneAssociation_Column()
	 * @model
	 * @generated
	 */
	DBColumn getColumn();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.OneToOneAssociation#getColumn <em>Column</em>}' reference
	 * @param value the new value of the '<em>Column</em>' reference
	 * @see #getColumn()
	 * @generated
	 */
	void setColumn(DBColumn value);

	/**
	 * @return true if this association is bidirectional
	 * @generated not
	 */
	boolean isBidirectional();

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.OneToOneAssociation#getReverseAssociation()
	 * @generated not
	 */
	@Override
	OneToOneAssociation getReverseAssociation();

}
