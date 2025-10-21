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

import net.codecadenza.eclipse.model.project.IntegrationModule;

/**
 * A representation of the model object '<em><b>Integration Test Module</b></em>'.
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.testing.IntegrationTestModule#getDefaultTimeout <em>Default Timeout</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.IntegrationTestModule#getDecimalFormat <em>Decimal Format</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.IntegrationTestModule#getDateTimeFormat <em>Date Time Format</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.IntegrationTestModule#getIntegrationModule <em>Integration Module</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.IntegrationTestModule#getDateFormat <em>Date Format</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.IntegrationTestModule#getDecimalSeparator <em>Decimal Separator</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.IntegrationTestModule#getGroupingSeparator <em>Grouping Separator</em>}</li>
 * </ul>
 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getIntegrationTestModule()
 * @model
 * @generated
 */
public interface IntegrationTestModule extends AbstractTestModule {
	/**
	 * Return the value of the '<em><b>Default Timeout</b></em>' attribute
	 * @return the value of the '<em>Default Timeout</em>' attribute
	 * @see #setDefaultTimeout(Integer)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getIntegrationTestModule_DefaultTimeout()
	 * @model
	 * @generated
	 */
	Integer getDefaultTimeout();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.IntegrationTestModule#getDefaultTimeout <em>Default
	 * Timeout</em>}' attribute
	 * @param value the new value of the '<em>Default Timeout</em>' attribute
	 * @see #getDefaultTimeout()
	 * @generated
	 */
	void setDefaultTimeout(Integer value);

	/**
	 * Return the value of the '<em><b>Decimal Format</b></em>' attribute
	 * @return the value of the '<em>Decimal Format</em>' attribute
	 * @see #setDecimalFormat(String)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getIntegrationTestModule_DecimalFormat()
	 * @model
	 * @generated
	 */
	String getDecimalFormat();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.IntegrationTestModule#getDecimalFormat <em>Decimal
	 * Format</em>}' attribute
	 * @param value the new value of the '<em>Decimal Format</em>' attribute
	 * @see #getDecimalFormat()
	 * @generated
	 */
	void setDecimalFormat(String value);

	/**
	 * Return the value of the '<em><b>Date Time Format</b></em>' attribute
	 * @return the value of the '<em>Date Time Format</em>' attribute
	 * @see #setDateTimeFormat(String)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getIntegrationTestModule_DateTimeFormat()
	 * @model
	 * @generated
	 */
	String getDateTimeFormat();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.IntegrationTestModule#getDateTimeFormat <em>Date Time
	 * Format</em>}' attribute
	 * @param value the new value of the '<em>Date Time Format</em>' attribute
	 * @see #getDateTimeFormat()
	 * @generated
	 */
	void setDateTimeFormat(String value);

	/**
	 * Return the value of the '<em><b>Integration Module</b></em>' reference
	 * @return the value of the '<em>Integration Module</em>' reference
	 * @see #setIntegrationModule(IntegrationModule)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getIntegrationTestModule_IntegrationModule()
	 * @model
	 * @generated
	 */
	IntegrationModule getIntegrationModule();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.IntegrationTestModule#getIntegrationModule <em>Integration
	 * Module</em>}' reference
	 * @param value the new value of the '<em>Integration Module</em>' reference
	 * @see #getIntegrationModule()
	 * @generated
	 */
	void setIntegrationModule(IntegrationModule value);

	/**
	 * Return the value of the '<em><b>Date Format</b></em>' attribute
	 * @return the value of the '<em>Date Format</em>' attribute
	 * @see #setDateFormat(String)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getIntegrationTestModule_DateFormat()
	 * @model
	 * @generated
	 */
	String getDateFormat();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.IntegrationTestModule#getDateFormat <em>Date Format</em>}'
	 * attribute
	 * @param value the new value of the '<em>Date Format</em>' attribute
	 * @see #getDateFormat()
	 * @generated
	 */
	void setDateFormat(String value);

	/**
	 * Return the value of the '<em><b>Decimal Separator</b></em>' attribute
	 * @return the value of the '<em>Decimal Separator</em>' attribute
	 * @see #setDecimalSeparator(char)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getIntegrationTestModule_DecimalSeparator()
	 * @model
	 * @generated
	 */
	char getDecimalSeparator();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.IntegrationTestModule#getDecimalSeparator <em>Decimal
	 * Separator</em>}' attribute
	 * @param value the new value of the '<em>Decimal Separator</em>' attribute
	 * @see #getDecimalSeparator()
	 * @generated
	 */
	void setDecimalSeparator(char value);

	/**
	 * Return the value of the '<em><b>Grouping Separator</b></em>' attribute
	 * @return the value of the '<em>Grouping Separator</em>' attribute
	 * @see #setGroupingSeparator(char)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getIntegrationTestModule_GroupingSeparator()
	 * @model
	 * @generated
	 */
	char getGroupingSeparator();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.IntegrationTestModule#getGroupingSeparator <em>Grouping
	 * Separator</em>}' attribute
	 * @param value the new value of the '<em>Grouping Separator</em>' attribute
	 * @see #getGroupingSeparator()
	 * @generated
	 */
	void setGroupingSeparator(char value);

}
