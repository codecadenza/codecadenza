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
 * A representation of the model object '<em><b>Enum Literal</b></em>'.
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.java.EnumLiteral#getName <em>Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.java.EnumLiteral#getTag <em>Tag</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.java.EnumLiteral#getJavaEnum <em>Java Enum</em>}</li>
 * </ul>
 * </p>
 * @see net.codecadenza.eclipse.model.java.JavaPackage#getEnumLiteral()
 * @model
 * @generated
 */
public interface EnumLiteral extends EObject {
	/**
	 * Return the value of the '<em><b>Name</b></em>' attribute
	 * @return the value of the '<em>Name</em>' attribute
	 * @see #setName(String)
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getEnumLiteral_Name()
	 * @model
	 * @generated
	 */
	String getName();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.java.EnumLiteral#getName <em>Name</em>}' attribute
	 * @param value the new value of the '<em>Name</em>' attribute
	 * @see #getName()
	 * @generated
	 */
	void setName(String value);

	/**
	 * Return the value of the '<em><b>Tag</b></em>' attribute. The literals are from the enumeration
	 * {@link net.codecadenza.eclipse.model.java.EnumLiteralTagEnumeration}.
	 * @return the value of the '<em>Tag</em>' attribute
	 * @see net.codecadenza.eclipse.model.java.EnumLiteralTagEnumeration
	 * @see #setTag(EnumLiteralTagEnumeration)
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getEnumLiteral_Tag()
	 * @model
	 * @generated
	 */
	EnumLiteralTagEnumeration getTag();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.java.EnumLiteral#getTag <em>Tag</em>}' attribute
	 * @param value the new value of the '<em>Tag</em>' attribute
	 * @see net.codecadenza.eclipse.model.java.EnumLiteralTagEnumeration
	 * @see #getTag()
	 * @generated
	 */
	void setTag(EnumLiteralTagEnumeration value);

	/**
	 * Return the value of the '<em><b>Java Enum</b></em>' container reference. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.java.JavaEnum#getEnumerationValues <em>Enumeration Values</em>}'.
	 * @return the value of the '<em>Java Enum</em>' container reference
	 * @see #setJavaEnum(JavaEnum)
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getEnumLiteral_JavaEnum()
	 * @see net.codecadenza.eclipse.model.java.JavaEnum#getEnumerationValues
	 * @model opposite="enumerationValues" transient="false"
	 * @generated
	 */
	JavaEnum getJavaEnum();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.java.EnumLiteral#getJavaEnum <em>Java Enum</em>}' container
	 * reference
	 * @param value the new value of the '<em>Java Enum</em>' container reference
	 * @see #getJavaEnum()
	 * @generated
	 */
	void setJavaEnum(JavaEnum value);

}
