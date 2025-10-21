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

import net.codecadenza.eclipse.model.mapping.MappingObject;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;

/**
 * A representation of the model object '<em><b>Test Data Object</b></em>'.
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.testing.TestDataObject#getMappingObject <em>Mapping Object</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.TestDataObject#getAttributes <em>Attributes</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.TestDataObject#getReferencedObject <em>Referenced Object</em>}</li>
 * </ul>
 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getTestDataObject()
 * @model
 * @generated
 */
public interface TestDataObject extends EObject {
	/**
	 * Return the value of the '<em><b>Mapping Object</b></em>' reference
	 * @return the value of the '<em>Mapping Object</em>' reference
	 * @see #setMappingObject(MappingObject)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getTestDataObject_MappingObject()
	 * @model
	 * @generated
	 */
	MappingObject getMappingObject();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.TestDataObject#getMappingObject <em>Mapping Object</em>}'
	 * reference
	 * @param value the new value of the '<em>Mapping Object</em>' reference
	 * @see #getMappingObject()
	 * @generated
	 */
	void setMappingObject(MappingObject value);

	/**
	 * Return the value of the '<em><b>Attributes</b></em>' containment reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.testing.TestDataAttribute}.
	 * @return the value of the '<em>Attributes</em>' containment reference list
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getTestDataObject_Attributes()
	 * @model containment="true"
	 * @generated
	 */
	EList<TestDataAttribute> getAttributes();

	/**
	 * Return the value of the '<em><b>Referenced Object</b></em>' reference
	 * @return the value of the '<em>Referenced Object</em>' reference
	 * @see #setReferencedObject(TestDataObject)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getTestDataObject_ReferencedObject()
	 * @model
	 * @generated
	 */
	TestDataObject getReferencedObject();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.TestDataObject#getReferencedObject <em>Referenced
	 * Object</em>}' reference
	 * @param value the new value of the '<em>Referenced Object</em>' reference
	 * @see #getReferencedObject()
	 * @generated
	 */
	void setReferencedObject(TestDataObject value);

	/**
	 * @return the primary key attribute or null if no primary key attribute exists
	 * @generated not
	 */
	TestDataAttribute getPKAttribute();

	/**
	 * @return the display attribute or null if no display attribute exists
	 * @generated not
	 */
	TestDataAttribute getDisplayAttribute();

	/**
	 * @param name the name to search for
	 * @return the {@link TestDataAttribute} or null if an attribute with this name could not be found
	 * @generated not
	 */
	TestDataAttribute getAttributeByName(String name);

}
