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
package net.codecadenza.eclipse.model.db;

import net.codecadenza.eclipse.model.java.JavaType;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;

/**
 * A representation of the model object '<em><b>DB Column Type</b></em>'.
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.db.DBColumnType#getJavaTypes <em>Java Types</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.DBColumnType#getName <em>Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.db.DBColumnType#isOmitSizeInformation <em>Omit Size Information</em>}</li>
 * </ul>
 * </p>
 * @see net.codecadenza.eclipse.model.db.DbPackage#getDBColumnType()
 * @model
 * @generated
 */
public interface DBColumnType extends EObject {
	/**
	 * Return the value of the '<em><b>Name</b></em>' attribute
	 * @return the value of the '<em>Name</em>' attribute
	 * @see #setName(String)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDBColumnType_Name()
	 * @model
	 * @generated
	 */
	String getName();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.db.DBColumnType#getName <em>Name</em>}' attribute
	 * @param value the new value of the '<em>Name</em>' attribute
	 * @see #getName()
	 * @generated
	 */
	void setName(String value);

	/**
	 * Return the value of the '<em><b>Omit Size Information</b></em>' attribute
	 * @return the value of the '<em>Omit Size Information</em>' attribute
	 * @see #setOmitSizeInformation(boolean)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDBColumnType_OmitSizeInformation()
	 * @model
	 * @generated
	 */
	boolean isOmitSizeInformation();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.db.DBColumnType#isOmitSizeInformation <em>Omit Size
	 * Information</em>}' attribute
	 * @param value the new value of the '<em>Omit Size Information</em>' attribute
	 * @see #isOmitSizeInformation()
	 * @generated
	 */
	void setOmitSizeInformation(boolean value);

	/**
	 * Return the value of the '<em><b>Java Types</b></em>' reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.java.JavaType}.
	 * @return the value of the '<em>Java Types</em>' reference list
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDBColumnType_JavaTypes()
	 * @model
	 * @generated
	 */
	EList<JavaType> getJavaTypes();

}
