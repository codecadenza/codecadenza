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

import net.codecadenza.eclipse.model.service.ServiceMethod;

/**
 * A representation of the model object '<em><b>Repository Method</b></em>'.
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.repository.RepositoryMethod#getRepository <em>Repository</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.repository.RepositoryMethod#getMethodType <em>Method Type</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.repository.RepositoryMethod#getHint <em>Hint</em>}</li>
 * </ul>
 * </p>
 * @see net.codecadenza.eclipse.model.repository.RepositoryPackage#getRepositoryMethod()
 * @model
 * @generated
 */
public interface RepositoryMethod extends ServiceMethod {
	/**
	 * Return the value of the '<em><b>Repository</b></em>' container reference. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.repository.Repository#getRepositoryMethods <em>Repository Methods</em>}'.
	 * @return the value of the '<em>Repository</em>' container reference
	 * @see #setRepository(Repository)
	 * @see net.codecadenza.eclipse.model.repository.RepositoryPackage#getRepositoryMethod_Repository()
	 * @see net.codecadenza.eclipse.model.repository.Repository#getRepositoryMethods
	 * @model opposite="repositoryMethods"
	 * @generated
	 */
	Repository getRepository();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.repository.RepositoryMethod#getRepository <em>Repository</em>}'
	 * container reference
	 * @param value the new value of the '<em>Repository</em>' container reference
	 * @see #getRepository()
	 * @generated
	 */
	void setRepository(Repository value);

	/**
	 * Return the value of the '<em><b>Method Type</b></em>' attribute. The literals are from the enumeration
	 * {@link net.codecadenza.eclipse.model.repository.RepositoryMethodTypeEnumeration}.
	 * @return the value of the '<em>Method Type</em>' attribute
	 * @see net.codecadenza.eclipse.model.repository.RepositoryMethodTypeEnumeration
	 * @see #setMethodType(RepositoryMethodTypeEnumeration)
	 * @see net.codecadenza.eclipse.model.repository.RepositoryPackage#getRepositoryMethod_MethodType()
	 * @model
	 * @generated
	 */
	RepositoryMethodTypeEnumeration getMethodType();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.repository.RepositoryMethod#getMethodType <em>Method Type</em>}'
	 * attribute
	 * @param value the new value of the '<em>Method Type</em>' attribute
	 * @see net.codecadenza.eclipse.model.repository.RepositoryMethodTypeEnumeration
	 * @see #getMethodType()
	 * @generated
	 */
	void setMethodType(RepositoryMethodTypeEnumeration value);

	/**
	 * Return the value of the '<em><b>Hint</b></em>' attribute
	 * @return the value of the '<em>Hint</em>' attribute
	 * @see #setHint(String)
	 * @see net.codecadenza.eclipse.model.repository.RepositoryPackage#getRepositoryMethod_Hint()
	 * @model
	 * @generated
	 */
	String getHint();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.repository.RepositoryMethod#getHint <em>Hint</em>}' attribute
	 * @param value the new value of the '<em>Hint</em>' attribute
	 * @see #getHint()
	 * @generated
	 */
	void setHint(String value);

	/**
	 * @return true if this repository method performs a unique key check
	 * @generated not
	 */
	boolean addUniqueCheck();

	/**
	 * @return true if this method needs an additional parameter representing the ID of the logged on user
	 * @generated not
	 */
	boolean addUserParam();

	/**
	 * @return true if this method should not be generated
	 * @generated not
	 */
	boolean isGenerationOmitted();

}
