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
package net.codecadenza.eclipse.model.repository;

import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.service.ServiceBean;
import org.eclipse.emf.common.util.EList;

/**
 * A representation of the model object '<em><b>Repository</b></em>'.
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.repository.Repository#getRepositoryMethods <em>Repository Methods</em>}</li>
 * </ul>
 * </p>
 * @see net.codecadenza.eclipse.model.repository.RepositoryPackage#getRepository()
 * @model
 * @generated
 */
public interface Repository extends ServiceBean {
	/**
	 * Return the value of the '<em><b>Repository Methods</b></em>' containment reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.repository.RepositoryMethod}. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.repository.RepositoryMethod#getRepository <em>Repository</em>}'.
	 * @return the value of the '<em>Repository Methods</em>' containment reference list
	 * @see net.codecadenza.eclipse.model.repository.RepositoryPackage#getRepository_RepositoryMethods()
	 * @see net.codecadenza.eclipse.model.repository.RepositoryMethod#getRepository
	 * @model opposite="repository" containment="true"
	 * @generated
	 */
	EList<RepositoryMethod> getRepositoryMethods();

	/**
	 * Get a repository method by the given type
	 * @param type
	 * @return a method that has this type
	 * @generated not
	 */
	RepositoryMethod getMethodByType(RepositoryMethodTypeEnumeration type);

	/**
	 * Get a repository method by the given type and an association that is referenced by a parameter
	 * @param type
	 * @param association
	 * @return a method that has this type and references the given association or null if no such method exists
	 * @generated not
	 */
	RepositoryMethod getMethodByTypeAndAssociation(RepositoryMethodTypeEnumeration type, AbstractDomainAssociation association);

	/**
	 * @return the internal representation of the repository source file
	 * @generated not
	 */
	JavaFile getSourceFile();
}
