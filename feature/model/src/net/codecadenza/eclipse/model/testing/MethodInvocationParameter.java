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
package net.codecadenza.eclipse.model.testing;

import net.codecadenza.eclipse.model.java.JavaType;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;

/**
 * A representation of the model object '<em><b>Method Invocation Parameter</b></em>'.
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.testing.MethodInvocationParameter#getParameterValues <em>Parameter Values</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.MethodInvocationParameter#getName <em>Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.MethodInvocationParameter#getType <em>Type</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.MethodInvocationParameter#isRepresentsList <em>Represents List</em>}</li>
 * </ul>
 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getMethodInvocationParameter()
 * @model
 * @generated
 */
public interface MethodInvocationParameter extends EObject {
	/**
	 * Return the value of the '<em><b>Parameter Values</b></em>' containment reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.testing.TestDataObject}.
	 * @return the value of the '<em>Parameter Values</em>' containment reference list
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getMethodInvocationParameter_ParameterValues()
	 * @model containment="true"
	 * @generated
	 */
	EList<TestDataObject> getParameterValues();

	/**
	 * Return the value of the '<em><b>Name</b></em>' attribute
	 * @return the value of the '<em>Name</em>' attribute
	 * @see #setName(String)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getMethodInvocationParameter_Name()
	 * @model
	 * @generated
	 */
	String getName();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.MethodInvocationParameter#getName <em>Name</em>}'
	 * attribute
	 * @param value the new value of the '<em>Name</em>' attribute
	 * @see #getName()
	 * @generated
	 */
	void setName(String value);

	/**
	 * Return the value of the '<em><b>Type</b></em>'
	 * @return the value of the '<em>Type</em>' reference
	 * @see #setType(JavaType)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getMethodInvocationParameter_Type()
	 * @model
	 * @generated
	 */
	JavaType getType();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.MethodInvocationParameter#getType <em>Type</em>}'
	 * reference
	 * @param value the new value of the '<em>Type</em>' reference
	 * @see #getType()
	 * @generated
	 */
	void setType(JavaType value);

	/**
	 * Return the value of the '<em><b>Represents List</b></em>' attribute
	 * @return the value of the '<em>Represents List</em>' attribute
	 * @see #setRepresentsList(boolean)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getMethodInvocationParameter_RepresentsList()
	 * @model
	 * @generated
	 */
	boolean isRepresentsList();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.MethodInvocationParameter#isRepresentsList <em>Represents
	 * List</em>}' attribute
	 * @param value the new value of the '<em>Represents List</em>' attribute
	 * @see #isRepresentsList()
	 * @generated
	 */
	void setRepresentsList(boolean value);

}
