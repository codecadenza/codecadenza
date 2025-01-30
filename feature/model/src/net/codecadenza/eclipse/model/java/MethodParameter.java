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
package net.codecadenza.eclipse.model.java;

import org.eclipse.emf.ecore.EObject;

/**
 * A representation of the model object '<em><b>Method Parameter</b></em>'.
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.java.MethodParameter#getName <em>Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.java.MethodParameter#getMethod <em>Method</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.java.MethodParameter#getType <em>Java Type</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.java.MethodParameter#getModifier <em>Modifier</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.java.MethodParameter#getHint <em>Hint</em>}</li>
 * </ul>
 * </p>
 * @see net.codecadenza.eclipse.model.java.JavaPackage#getMethodParameter()
 * @model
 * @generated
 */
public interface MethodParameter extends EObject {
	/**
	 * Return the value of the '<em><b>Name</b></em>' attribute
	 * @return the value of the '<em>Name</em>' attribute
	 * @see #setName(String)
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getMethodParameter_Name()
	 * @model
	 * @generated
	 */
	String getName();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.java.MethodParameter#getName <em>Name</em>}' attribute
	 * @param value the new value of the '<em>Name</em>' attribute
	 * @see #getName()
	 * @generated
	 */
	void setName(String value);

	/**
	 * Return the value of the '<em><b>Java Method</b></em>' container reference. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.java.JavaMethod#getMethodParameters <em>Method Parameters</em>}'.
	 * @return the value of the '<em>Java Method</em>' container reference
	 * @see #setMethod(JavaMethod)
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getMethodParameter_Method()
	 * @see net.codecadenza.eclipse.model.java.JavaMethod#getMethodParameters
	 * @model opposite="methodParameters"
	 * @generated
	 */
	JavaMethod getMethod();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.java.MethodParameter#getMethod <em>Java Method</em>}' container
	 * reference
	 * @param value the new value of the '<em>Java Method</em>' container reference
	 * @see #getMethod()
	 * @generated
	 */
	void setMethod(JavaMethod value);

	/**
	 * Return the value of the '<em><b>Java Type</b></em>' reference
	 * @return the value of the '<em>Java Type</em>' reference
	 * @see #setType(JavaType)
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getMethodParameter_Type()
	 * @model
	 * @generated
	 */
	JavaType getType();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.java.MethodParameter#getType <em>Java Type</em>}' reference
	 * @param value the new value of the '<em>Java Type</em>' reference
	 * @see #getType()
	 * @generated
	 */
	void setType(JavaType value);

	/**
	 * Return the value of the '<em><b>Modifier</b></em>' attribute. The literals are from the enumeration
	 * {@link net.codecadenza.eclipse.model.java.JavaTypeModifierEnumeration}.
	 * @return the value of the '<em>Modifier</em>' attribute
	 * @see net.codecadenza.eclipse.model.java.JavaTypeModifierEnumeration
	 * @see #setModifier(JavaTypeModifierEnumeration)
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getMethodParameter_Modifier()
	 * @model
	 * @generated
	 */
	JavaTypeModifierEnumeration getModifier();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.java.MethodParameter#getModifier <em>Modifier</em>}' attribute
	 * @param value the new value of the '<em>Modifier</em>' attribute
	 * @see net.codecadenza.eclipse.model.java.JavaTypeModifierEnumeration
	 * @see #getModifier()
	 * @generated
	 */
	void setModifier(JavaTypeModifierEnumeration value);

	/**
	 * Return the value of the '<em><b>Hint</b></em>' attribute
	 * @return the value of the '<em>Hint</em>' attribute
	 * @see #setHint(String)
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getMethodParameter_Hint()
	 * @model
	 * @generated
	 */
	String getHint();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.java.MethodParameter#getHint <em>Hint</em>}' attribute
	 * @param value the new value of the '<em>Hint</em>' attribute
	 * @see #getHint()
	 * @generated
	 */
	void setHint(String value);

}
