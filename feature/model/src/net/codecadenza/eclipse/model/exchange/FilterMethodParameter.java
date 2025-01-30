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
package net.codecadenza.eclipse.model.exchange;

import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.java.MethodParameter;
import org.eclipse.emf.common.util.EList;

/**
 * A representation of the model object '<em><b>Filter Method Parameter</b></em>'.
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.exchange.FilterMethodParameter#getDomainAttribute <em>Domain Attribute</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.FilterMethodParameter#getAssociation <em>Association</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.FilterMethodParameter#getAssociationList <em>Association List</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.FilterMethodParameter#getOperator <em>Operator</em>}</li>
 * </ul>
 * </p>
 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getFilterMethodParameter()
 * @model
 * @generated
 */
public interface FilterMethodParameter extends MethodParameter {
	/**
	 * Return the value of the '<em><b>Domain Attribute</b></em>' reference
	 * @return the value of the '<em>Domain Attribute</em>' reference
	 * @see #setDomainAttribute(DomainAttribute)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getFilterMethodParameter_DomainAttribute()
	 * @model
	 * @generated
	 */
	DomainAttribute getDomainAttribute();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.FilterMethodParameter#getDomainAttribute <em>Domain
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
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getFilterMethodParameter_Association()
	 * @model
	 * @generated
	 */
	AbstractDomainAssociation getAssociation();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.FilterMethodParameter#getAssociation
	 * <em>Association</em>}' reference
	 * @param value the new value of the '<em>Association</em>' reference
	 * @see #getAssociation()
	 * @generated
	 */
	void setAssociation(AbstractDomainAssociation value);

	/**
	 * Return the value of the '<em><b>Association List</b></em>' reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.domain.AbstractDomainAssociation}.
	 * @return the value of the '<em>Association List</em>' reference list
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getFilterMethodParameter_AssociationList()
	 * @model
	 * @generated
	 */
	EList<AbstractDomainAssociation> getAssociationList();

	/**
	 * Return the value of the '<em><b>Operator</b></em>' attribute
	 * @return the value of the '<em>Operator</em>' attribute
	 * @see #setOperator(String)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getFilterMethodParameter_Operator()
	 * @model
	 * @generated
	 */
	String getOperator();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.FilterMethodParameter#getOperator <em>Operator</em>}'
	 * attribute
	 * @param value the new value of the '<em>Operator</em>' attribute
	 * @see #getOperator()
	 * @generated
	 */
	void setOperator(String value);

}
