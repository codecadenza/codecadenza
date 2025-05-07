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
package net.codecadenza.eclipse.model.boundary;

import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.service.ServiceMethod;

/**
 * A representation of the model object '<em><b>Boundary Method</b></em>'.
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.boundary.BoundaryMethod#getBoundaryBean <em>Boundary Bean</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.boundary.BoundaryMethod#getMethodType <em>Method Type</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.boundary.BoundaryMethod#getDomainAttribute <em>Domain Attribute</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.boundary.BoundaryMethod#getAssociation <em>Association</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.boundary.BoundaryMethod#getDataFetchType <em>Data Fetch Type</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.boundary.BoundaryMethod#getServiceMethod <em>Service Method</em>}</li>
 * </ul>
 * </p>
 * @see net.codecadenza.eclipse.model.boundary.BoundaryPackage#getBoundaryMethod()
 * @model
 * @generated
 */
public interface BoundaryMethod extends ServiceMethod {
	/**
	 * Return the value of the '<em><b>Boundary Bean</b></em>' container reference. It is bidirectional and its opposite is '
	 * {@link net.codecadenza.eclipse.model.boundary.BoundaryBean#getBoundaryMethods <em>Boundary Methods</em>}'.
	 * @return the value of the '<em>Boundary Bean</em>' container reference
	 * @see #setBoundaryBean(BoundaryBean)
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryPackage#getBoundaryMethod_BoundaryBean()
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryBean#getBoundaryMethods
	 * @model opposite="boundaryMethods"
	 * @generated
	 */
	BoundaryBean getBoundaryBean();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.boundary.BoundaryMethod#getBoundaryBean <em>Boundary Bean</em>}'
	 * container reference
	 * @param value the new value of the '<em>Boundary Bean</em>' container reference
	 * @see #getBoundaryBean()
	 * @generated
	 */
	void setBoundaryBean(BoundaryBean value);

	/**
	 * Return the value of the '<em><b>Method Type</b></em>' attribute. The literals are from the enumeration
	 * {@link net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration}.
	 * @return the value of the '<em>Method Type</em>' attribute
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration
	 * @see #setMethodType(BoundaryMethodTypeEnumeration)
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryPackage#getBoundaryMethod_MethodType()
	 * @model
	 * @generated
	 */
	BoundaryMethodTypeEnumeration getMethodType();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.boundary.BoundaryMethod#getMethodType <em>Method Type</em>}'
	 * attribute
	 * @param value the new value of the '<em>Method Type</em>' attribute
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration
	 * @see #getMethodType()
	 * @generated
	 */
	void setMethodType(BoundaryMethodTypeEnumeration value);

	/**
	 * Return the value of the '<em><b>Domain Attribute</b></em>' reference
	 * @return the value of the '<em>Domain Attribute</em>' reference
	 * @see #setDomainAttribute(DomainAttribute)
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryPackage#getBoundaryMethod_DomainAttribute()
	 * @model
	 * @generated
	 */
	DomainAttribute getDomainAttribute();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.boundary.BoundaryMethod#getDomainAttribute <em>Domain
	 * Attribute</em>}' reference
	 * @param value the new value of the '<em>Domain Attribute</em>' reference
	 * @see #getDomainAttribute()
	 * @generated
	 */
	void setDomainAttribute(DomainAttribute value);

	/**
	 * Return the value of the '<em><b>Association</b></em>' reference
	 * @return the value of the '<em>Association</em>' reference
	 * @see #setAssociation(AbstractDomainAssociation)
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryPackage#getBoundaryMethod_Association()
	 * @model
	 * @generated
	 */
	AbstractDomainAssociation getAssociation();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.boundary.BoundaryMethod#getAssociation <em>Association</em>}'
	 * reference
	 * @param value the new value of the '<em>Association</em>' reference
	 * @see #getAssociation()
	 * @generated
	 */
	void setAssociation(AbstractDomainAssociation value);

	/**
	 * Return the value of the '<em><b>Data Fetch Type</b></em>' attribute. The literals are from the enumeration
	 * {@link net.codecadenza.eclipse.model.boundary.BoundaryMethodDataFetchType}.
	 * @return the value of the '<em>Data Fetch Type</em>' attribute
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryMethodDataFetchType
	 * @see #setDataFetchType(BoundaryMethodDataFetchType)
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryPackage#getBoundaryMethod_DataFetchType()
	 * @model
	 * @generated
	 */
	BoundaryMethodDataFetchType getDataFetchType();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.boundary.BoundaryMethod#getDataFetchType <em>Data Fetch
	 * Type</em>}' attribute
	 * @param value the new value of the '<em>Data Fetch Type</em>' attribute
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryMethodDataFetchType
	 * @see #getDataFetchType()
	 * @generated
	 */
	void setDataFetchType(BoundaryMethodDataFetchType value);

	/**
	 * Return the value of the '<em><b>Service Method</b></em>' reference
	 * @return the value of the '<em>Service Method</em>' reference
	 * @see #setServiceMethod(ServiceMethod)
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryPackage#getBoundaryMethod_ServiceMethod()
	 * @model
	 * @generated
	 */
	ServiceMethod getServiceMethod();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.boundary.BoundaryMethod#getServiceMethod <em>Service Method</em>}'
	 * reference
	 * @param value the new value of the '<em>Service Method</em>' reference
	 * @see #getServiceMethod()
	 * @generated
	 */
	void setServiceMethod(ServiceMethod value);

	/**
	 * @return true if this boundary method performs a unique key check
	 * @generated not
	 */
	boolean addUniqueCheck();

	/**
	 * @return true if this boundary method is used by an integration method
	 * @generated not
	 */
	boolean isUsedByIntegrationMethod();

	/**
	 * @return true if the method's DTO return type should be used when generating a facade method
	 * @generated not
	 */
	boolean useDTOReturnType();

	/**
	 * @return true if the facade method won't be added to the corresponding facade bean source file!
	 * @generated not
	 */
	boolean isVirtual();

	/**
	 * @return a corresponding 'SEARCH' method for a given 'COUNT' method. It will return null if no 'SEARCH' method has been found!
	 * @generated not
	 */
	BoundaryMethod getSearchMethod();

	/**
	 * @return a domain attribute that is used as additional filter criterion for user or client-related queries. The method will
	 *         return null if no such filter criterion is required!
	 * @generated not
	 */
	DomainAttribute getAdditionalFilterAttribute();

}
