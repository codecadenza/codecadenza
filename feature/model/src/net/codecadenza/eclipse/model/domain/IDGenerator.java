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

import org.eclipse.emf.ecore.EObject;

/**
 * A representation of the model object '<em><b>ID Generator</b></em>'.
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.domain.IDGenerator#getName <em>Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.IDGenerator#getBlockSize <em>Block Size</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.IDGenerator#getInitialValue <em>Initial Value</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.IDGenerator#getGeneratorType <em>Generator Type</em>}</li>
 * </ul>
 * </p>
 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getIDGenerator()
 * @model
 * @generated
 */
public interface IDGenerator extends EObject {
	/**
	 * Return the value of the '<em><b>Name</b></em>' attribute
	 * @return the value of the '<em>Name</em>' attribute
	 * @see #setName(String)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getIDGenerator_Name()
	 * @model
	 * @generated
	 */
	String getName();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.IDGenerator#getName <em>Name</em>}' attribute
	 * @param value the new value of the '<em>Name</em>' attribute
	 * @see #getName()
	 * @generated
	 */
	void setName(String value);

	/**
	 * Return the value of the '<em><b>Block Size</b></em>' attribute
	 * @return the value of the '<em>Block Size</em>' attribute
	 * @see #setBlockSize(int)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getIDGenerator_BlockSize()
	 * @model
	 * @generated
	 */
	int getBlockSize();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.IDGenerator#getBlockSize <em>Block Size</em>}' attribute
	 * @param value the new value of the '<em>Block Size</em>' attribute
	 * @see #getBlockSize()
	 * @generated
	 */
	void setBlockSize(int value);

	/**
	 * Return the value of the '<em><b>Initial Value</b></em>' attribute
	 * @return the value of the '<em>Initial Value</em>' attribute
	 * @see #setInitialValue(int)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getIDGenerator_InitialValue()
	 * @model
	 * @generated
	 */
	int getInitialValue();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.IDGenerator#getInitialValue <em>Initial Value</em>}'
	 * attribute
	 * @param value the new value of the '<em>Initial Value</em>' attribute
	 * @see #getInitialValue()
	 * @generated
	 */
	void setInitialValue(int value);

	/**
	 * Return the value of the '<em><b>Generator Type</b></em>' attribute. The literals are from the enumeration
	 * {@link net.codecadenza.eclipse.model.domain.IDGeneratorTypeEnumeration}.
	 * @return the value of the '<em>Generator Type</em>' attribute
	 * @see net.codecadenza.eclipse.model.domain.IDGeneratorTypeEnumeration
	 * @see #setGeneratorType(IDGeneratorTypeEnumeration)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getIDGenerator_GeneratorType()
	 * @model
	 * @generated
	 */
	IDGeneratorTypeEnumeration getGeneratorType();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.IDGenerator#getGeneratorType <em>Generator Type</em>}'
	 * attribute
	 * @param value the new value of the '<em>Generator Type</em>' attribute
	 * @see net.codecadenza.eclipse.model.domain.IDGeneratorTypeEnumeration
	 * @see #getGeneratorType()
	 * @generated
	 */
	void setGeneratorType(IDGeneratorTypeEnumeration value);

}
