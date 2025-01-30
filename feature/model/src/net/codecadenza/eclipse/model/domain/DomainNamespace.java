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

import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.model.java.Namespace;
import net.codecadenza.eclipse.model.project.WorkspaceFile;
import org.eclipse.emf.common.util.EList;

/**
 * A representation of the model object '<em><b>Namespace</b></em>'.
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.domain.DomainNamespace#getDomainObjects <em>Domain Objects</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.DomainNamespace#getEnumerations <em>Enumerations</em>}</li>
 * </ul>
 * </p>
 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainNamespace()
 * @model
 * @generated
 */
public interface DomainNamespace extends Namespace {
	/**
	 * Return the value of the '<em><b>Domain Objects</b></em>' containment reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.domain.DomainObject}.
	 * @return the value of the '<em>Domain Objects</em>' containment reference list
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainNamespace_DomainObjects()
	 * @model containment="true"
	 * @generated
	 */
	EList<DomainObject> getDomainObjects();

	/**
	 * Return the value of the '<em><b>Enumerations</b></em>' containment reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.java.JavaEnum}.
	 * @return the value of the '<em>Enumerations</em>' containment reference list
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainNamespace_Enumerations()
	 * @model containment="true"
	 * @generated
	 */
	EList<JavaEnum> getEnumerations();

	/**
	 * @return the internal representation of the respective CodeCadenza diagram file
	 * @generated not
	 */
	WorkspaceFile getDiagramFile();

}
