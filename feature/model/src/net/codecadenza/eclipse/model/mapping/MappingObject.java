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
package net.codecadenza.eclipse.model.mapping;

import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.java.JavaType;

/**
 * A representation of the model object '<em><b>Mapping Object</b></em>'.
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.mapping.MappingObject#getDomainObject <em>Domain Object</em>}</li>
 * </ul>
 * </p>
 * @see net.codecadenza.eclipse.model.mapping.MappingPackage#getMappingObject()
 * @model abstract="true"
 * @generated
 */
public interface MappingObject extends JavaType {
	/**
	 * Return the value of the '<em><b>Domain Object</b></em>' reference
	 * @return the value of the '<em>Domain Object</em>' reference
	 * @see #setDomainObject(DomainObject)
	 * @see net.codecadenza.eclipse.model.mapping.MappingPackage#getMappingObject_DomainObject()
	 * @model
	 * @generated
	 */
	DomainObject getDomainObject();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.mapping.MappingObject#getDomainObject <em>Domain Object</em>}'
	 * reference
	 * @param value the new value of the '<em>Domain Object</em>' reference
	 * @see #getDomainObject()
	 * @generated
	 */
	void setDomainObject(DomainObject value);

	/**
	 * @return the internal representation of the mapping object source file
	 * @generated not
	 */
	JavaFile getSourceFile();

}
