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

import org.eclipse.emf.ecore.EObject;

/**
 * A representation of the model object '<em><b>Attribute Validator</b></em>'.
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.domain.DomainAttributeValidator#isFutureDate <em>Future Date</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.DomainAttributeValidator#isPastDate <em>Past Date</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.DomainAttributeValidator#getMaxLength <em>Max Length</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.DomainAttributeValidator#getMinLength <em>Min Length</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.DomainAttributeValidator#isNullable <em>Nullable</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.DomainAttributeValidator#getMaxValue <em>Max Value</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.DomainAttributeValidator#getMinValue <em>Min Value</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.DomainAttributeValidator#getRegularExpression <em>Regular Expression</em>}</li>
 * </ul>
 * </p>
 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttributeValidator()
 * @model
 * @generated
 */
public interface DomainAttributeValidator extends EObject {
	/**
	 * Return the value of the '<em><b>Future Date</b></em>' attribute
	 * @return the value of the '<em>Future Date</em>' attribute
	 * @see #setFutureDate(boolean)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttributeValidator_FutureDate()
	 * @model
	 * @generated
	 */
	boolean isFutureDate();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.DomainAttributeValidator#isFutureDate <em>Future
	 * Date</em>}' attribute
	 * @param value the new value of the '<em>Future Date</em>' attribute
	 * @see #isFutureDate()
	 * @generated
	 */
	void setFutureDate(boolean value);

	/**
	 * Return the value of the '<em><b>Past Date</b></em>' attribute
	 * @return the value of the '<em>Past Date</em>' attribute
	 * @see #setPastDate(boolean)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttributeValidator_PastDate()
	 * @model
	 * @generated
	 */
	boolean isPastDate();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.DomainAttributeValidator#isPastDate <em>Past Date</em>}'
	 * attribute
	 * @param value the new value of the '<em>Past Date</em>' attribute
	 * @see #isPastDate()
	 * @generated
	 */
	void setPastDate(boolean value);

	/**
	 * Return the value of the '<em><b>Max Length</b></em>' attribute
	 * @return the value of the '<em>Max Length</em>' attribute
	 * @see #setMaxLength(Integer)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttributeValidator_MaxLength()
	 * @model
	 * @generated
	 */
	Integer getMaxLength();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.DomainAttributeValidator#getMaxLength <em>Max Length</em>}'
	 * attribute
	 * @param value the new value of the '<em>Max Length</em>' attribute
	 * @see #getMaxLength()
	 * @generated
	 */
	void setMaxLength(Integer value);

	/**
	 * Return the value of the '<em><b>Min Length</b></em>' attribute
	 * @return the value of the '<em>Min Length</em>' attribute
	 * @see #setMinLength(Integer)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttributeValidator_MinLength()
	 * @model
	 * @generated
	 */
	Integer getMinLength();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.DomainAttributeValidator#getMinLength <em>Min Length</em>}'
	 * attribute
	 * @param value the new value of the '<em>Min Length</em>' attribute
	 * @see #getMinLength()
	 * @generated
	 */
	void setMinLength(Integer value);

	/**
	 * Return the value of the '<em><b>Nullable</b></em>' attribute
	 * @return the value of the '<em>Nullable</em>' attribute
	 * @see #setNullable(boolean)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttributeValidator_Nullable()
	 * @model
	 * @generated
	 */
	boolean isNullable();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.DomainAttributeValidator#isNullable <em>Nullable</em>}'
	 * attribute
	 * @param value the new value of the '<em>Nullable</em>' attribute
	 * @see #isNullable()
	 * @generated
	 */
	void setNullable(boolean value);

	/**
	 * Return the value of the '<em><b>Max Value</b></em>' attribute
	 * @return the value of the '<em>Max Value</em>' attribute
	 * @see #setMaxValue(String)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttributeValidator_MaxValue()
	 * @model
	 * @generated
	 */
	String getMaxValue();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.DomainAttributeValidator#getMaxValue <em>Max Value</em>}'
	 * attribute
	 * @param value the new value of the '<em>Max Value</em>' attribute
	 * @see #getMaxValue()
	 * @generated
	 */
	void setMaxValue(String value);

	/**
	 * Return the value of the '<em><b>Min Value</b></em>' attribute
	 * @return the value of the '<em>Min Value</em>' attribute
	 * @see #setMinValue(String)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttributeValidator_MinValue()
	 * @model
	 * @generated
	 */
	String getMinValue();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.DomainAttributeValidator#getMinValue <em>Min Value</em>}'
	 * attribute
	 * @param value the new value of the '<em>Min Value</em>' attribute
	 * @see #getMinValue()
	 * @generated
	 */
	void setMinValue(String value);

	/**
	 * Return the value of the '<em><b>Regular Expression</b></em>' attribute
	 * @return the value of the '<em>Regular Expression</em>' attribute
	 * @see #setRegularExpression(String)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttributeValidator_RegularExpression()
	 * @model
	 * @generated
	 */
	String getRegularExpression();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.DomainAttributeValidator#getRegularExpression <em>Regular
	 * Expression</em>}' attribute
	 * @param value the new value of the '<em>Regular Expression</em>' attribute
	 * @see #getRegularExpression()
	 * @generated
	 */
	void setRegularExpression(String value);

}
