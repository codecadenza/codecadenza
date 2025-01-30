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

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;

/**
 * A representation of the model object '<em><b>Java Method</b></em>'.
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.java.JavaMethod#getName <em>Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.java.JavaMethod#getComment <em>Comment</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.java.JavaMethod#getJavaType <em>Java Type</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.java.JavaMethod#getReturnType <em>Return Type</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.java.JavaMethod#getReturnTypeModifier <em>Return Type Modifier</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.java.JavaMethod#getMethodParameters <em>Method Parameters</em>}</li>
 * </ul>
 * </p>
 * @see net.codecadenza.eclipse.model.java.JavaPackage#getJavaMethod()
 * @model
 * @generated
 */
public interface JavaMethod extends EObject {
	/**
	 * Return the value of the '<em><b>Name</b></em>' attribute
	 * @return the value of the '<em>Name</em>' attribute
	 * @see #setName(String)
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getJavaMethod_Name()
	 * @model
	 * @generated
	 */
	String getName();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.java.JavaMethod#getName <em>Name</em>}' attribute
	 * @param value the new value of the '<em>Name</em>' attribute
	 * @see #getName()
	 * @generated
	 */
	void setName(String value);

	/**
	 * Return the value of the '<em><b>Comment</b></em>' attribute
	 * @return the value of the '<em>Comment</em>' attribute
	 * @see #setComment(String)
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getJavaMethod_Comment()
	 * @model
	 * @generated
	 */
	String getComment();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.java.JavaMethod#getComment <em>Comment</em>}' attribute
	 * @param value the new value of the '<em>Comment</em>' attribute
	 * @see #getComment()
	 * @generated
	 */
	void setComment(String value);

	/**
	 * Return the value of the '<em><b>Java Type</b></em>' reference
	 * @return the value of the '<em>Java Type</em>' reference
	 * @see #setJavaType(JavaType)
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getJavaMethod_JavaType()
	 * @model
	 * @generated
	 */
	JavaType getJavaType();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.java.JavaMethod#getJavaType <em>Java Type</em>}' reference
	 * @param value the new value of the '<em>Java Type</em>' reference
	 * @see #getJavaType()
	 * @generated
	 */
	void setJavaType(JavaType value);

	/**
	 * Return the value of the '<em><b>Return Type</b></em>' reference
	 * @return the value of the '<em>Return Type</em>' reference
	 * @see #setReturnType(JavaType)
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getJavaMethod_ReturnType()
	 * @model
	 * @generated
	 */
	JavaType getReturnType();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.java.JavaMethod#getReturnType <em>Return Type</em>}' reference
	 * @param value the new value of the '<em>Return Type</em>' reference
	 * @see #getReturnType()
	 * @generated
	 */
	void setReturnType(JavaType value);

	/**
	 * Return the value of the '<em><b>Return Type Modifier</b></em>' attribute. The literals are from the enumeration
	 * {@link net.codecadenza.eclipse.model.java.JavaTypeModifierEnumeration}.
	 * @return the value of the '<em>Return Type Modifier</em>' attribute
	 * @see net.codecadenza.eclipse.model.java.JavaTypeModifierEnumeration
	 * @see #setReturnTypeModifier(JavaTypeModifierEnumeration)
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getJavaMethod_ReturnTypeModifier()
	 * @model
	 * @generated
	 */
	JavaTypeModifierEnumeration getReturnTypeModifier();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.java.JavaMethod#getReturnTypeModifier <em>Return Type
	 * Modifier</em>}' attribute
	 * @param value the new value of the '<em>Return Type Modifier</em>' attribute
	 * @see net.codecadenza.eclipse.model.java.JavaTypeModifierEnumeration
	 * @see #getReturnTypeModifier()
	 * @generated
	 */
	void setReturnTypeModifier(JavaTypeModifierEnumeration value);

	/**
	 * Return the value of the '<em><b>Method Parameters</b></em>' containment reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.java.MethodParameter}. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.java.MethodParameter#getMethod <em>Method</em>}'.
	 * @return the value of the '<em>Method Parameters</em>' containment reference list
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getJavaMethod_MethodParameters()
	 * @see net.codecadenza.eclipse.model.java.MethodParameter#getMethod
	 * @model opposite="method" containment="true"
	 * @generated
	 */
	EList<MethodParameter> getMethodParameters();

	/**
	 * @return the first method parameter
	 * @throws IllegalStateException if no parameter exists
	 * @generated not
	 */
	MethodParameter getFirstParameter();

	/**
	 * @param mustExist
	 * @return the first method parameter or null if no parameter has been found
	 * @throws IllegalStateException if no parameter exists and {@code mustExist} is true
	 * @generated not
	 */
	MethodParameter getFirstParameter(boolean mustExist);

	/**
	 * @param addParameters a flag that controls if the parameters should be added
	 * @return the begin of the Javadoc comment including the actual comment text
	 * @generated not
	 */
	String generateBeginOfJavadocComment(boolean addParameters);

	/**
	 * @return the begin of the Javadoc comment including the actual comment text. All parameters will be omitted!
	 * @generated not
	 */
	String generateBeginOfJavadocComment();

}
