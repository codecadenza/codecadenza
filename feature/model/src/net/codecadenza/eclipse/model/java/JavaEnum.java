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

import java.util.ArrayList;
import net.codecadenza.eclipse.model.project.WorkspaceFile;
import org.eclipse.emf.common.util.EList;

/**
 * A representation of the model object '<em><b>Java Enum</b></em>'.
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.java.JavaEnum#getEnumerationValues <em>Enumeration Values</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.java.JavaEnum#getTag <em>Tag</em>}</li>
 * </ul>
 * </p>
 * @see net.codecadenza.eclipse.model.java.JavaPackage#getJavaEnum()
 * @model
 * @generated
 */
public interface JavaEnum extends JavaType {
	/**
	 * Return the value of the '<em><b>Enumeration Values</b></em>' containment reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.java.EnumLiteral}. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.java.EnumLiteral#getJavaEnum <em>Java Enum</em>}'.
	 * @return the value of the '<em>Enumeration Values</em>' containment reference list
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getJavaEnum_EnumerationValues()
	 * @see net.codecadenza.eclipse.model.java.EnumLiteral#getJavaEnum
	 * @model opposite="javaEnum" containment="true"
	 * @generated
	 */
	EList<EnumLiteral> getEnumerationValues();

	/**
	 * Return the value of the '<em><b>Tag</b></em>' attribute. The literals are from the enumeration
	 * {@link net.codecadenza.eclipse.model.java.EnumTagEnumeration}.
	 * @return the value of the '<em>Tag</em>' attribute
	 * @see net.codecadenza.eclipse.model.java.EnumTagEnumeration
	 * @see #setTag(EnumTagEnumeration)
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getJavaEnum_Tag()
	 * @model
	 * @generated
	 */
	EnumTagEnumeration getTag();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.java.JavaEnum#getTag <em>Tag</em>}' attribute
	 * @param value the new value of the '<em>Tag</em>' attribute
	 * @see net.codecadenza.eclipse.model.java.EnumTagEnumeration
	 * @see #getTag()
	 * @generated
	 */
	void setTag(EnumTagEnumeration value);

	/**
	 * @return a list of all valid literal tags for this enumeration
	 * @generated not
	 */
	ArrayList<String> getValidLiteralsTagsOfEnum();

	/**
	 * @return the internal representation of the enum source file
	 * @generated not
	 */
	JavaFile getSourceFile();

	/**
	 * @return the internal representation of the source file for an Angular application
	 * @generated not
	 */
	WorkspaceFile getTypeScriptSourceFile();

}
