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
 * A representation of the model object '<em><b>Many To One Association</b></em>'.
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.domain.ManyToOneAssociation#isInsertable <em>Insertable</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.ManyToOneAssociation#isUpdatable <em>Updatable</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.ManyToOneAssociation#isOptional <em>Optional</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.ManyToOneAssociation#getColumn <em>Column</em>}</li>
 * </ul>
 * </p>
 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getManyToOneAssociation()
 * @model
 * @generated
 */
public interface ManyToOneAssociation extends AbstractDomainAssociation {
	/**
	 * Return the value of the '<em><b>Insertable</b></em>' attribute
	 * @return the value of the '<em>Insertable</em>' attribute
	 * @see #setInsertable(boolean)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getManyToOneAssociation_Insertable()
	 * @model
	 * @generated
	 */
	boolean isInsertable();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.ManyToOneAssociation#isInsertable <em>Insertable</em>}'
	 * attribute
	 * @param value the new value of the '<em>Insertable</em>' attribute
	 * @see #isInsertable()
	 * @generated
	 */
	void setInsertable(boolean value);

	/**
	 * Return the value of the '<em><b>Updatable</b></em>' attribute
	 * @return the value of the '<em>Updatable</em>' attribute
	 * @see #setUpdatable(boolean)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getManyToOneAssociation_Updatable()
	 * @model
	 * @generated
	 */
	boolean isUpdatable();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.ManyToOneAssociation#isUpdatable <em>Updatable</em>}'
	 * attribute
	 * @param value the new value of the '<em>Updatable</em>' attribute
	 * @see #isUpdatable()
	 * @generated
	 */
	void setUpdatable(boolean value);

	/**
	 * Return the value of the '<em><b>Optional</b></em>' attribute
	 * @return the value of the '<em>Optional</em>' attribute
	 * @see #setOptional(boolean)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getManyToOneAssociation_Optional()
	 * @model
	 * @generated
	 */
	boolean isOptional();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.ManyToOneAssociation#isOptional <em>Optional</em>}'
	 * attribute
	 * @param value the new value of the '<em>Optional</em>' attribute
	 * @see #isOptional()
	 * @generated
	 */
	void setOptional(boolean value);

	/**
	 * Return the value of the '<em><b>Column</b></em>' reference
	 * @return the value of the '<em>Column</em>' reference
	 * @see #setColumn(DBColumn)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getManyToOneAssociation_Column()
	 * @model
	 * @generated
	 */
	DBColumn getColumn();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.ManyToOneAssociation#getColumn <em>Column</em>}' reference
	 * @param value the new value of the '<em>Column</em>' reference
	 * @see #getColumn()
	 * @generated
	 */
	void setColumn(DBColumn value);

	/**
	 * @return the reverse one-to-many association if it exists, or null if it couldn't be found!
	 * @generated not
	 */
	@Override
	OneToManyAssociation getReverseAssociation();

}
