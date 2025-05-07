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
package net.codecadenza.eclipse.model.integration;

/**
 * A representation of the model object '<em><b>SOAP Integration Method</b></em>'.
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.integration.SOAPIntegrationMethod#getOperationName <em>Operation Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.integration.SOAPIntegrationMethod#isAddParameterAnnotations <em>Add Parameter
 * Annotations</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.integration.SOAPIntegrationMethod#getReturnValueName <em>Return Value Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.integration.SOAPIntegrationMethod#getReturnValuePartName <em>Return Value Part
 * Name</em>}</li>
 * </ul>
 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getSOAPIntegrationMethod()
 * @model
 * @generated
 */
public interface SOAPIntegrationMethod extends AbstractIntegrationMethod {
	/**
	 * Return the value of the '<em><b>Operation Name</b></em>' attribute
	 * @return the value of the '<em>Operation Name</em>' attribute
	 * @see #setOperationName(String)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getSOAPIntegrationMethod_OperationName()
	 * @model
	 * @generated
	 */
	String getOperationName();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.integration.SOAPIntegrationMethod#getOperationName <em>Operation
	 * Name</em>}' attribute
	 * @param value the new value of the '<em>Operation Name</em>' attribute
	 * @see #getOperationName()
	 * @generated
	 */
	void setOperationName(String value);

	/**
	 * Return the value of the '<em><b>Add Parameter Annotations</b></em>' attribute
	 * @return the value of the '<em>Add Parameter Annotations</em>' attribute
	 * @see #setAddParameterAnnotations(boolean)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getSOAPIntegrationMethod_AddParameterAnnotations()
	 * @model
	 * @generated
	 */
	boolean isAddParameterAnnotations();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.integration.SOAPIntegrationMethod#isAddParameterAnnotations
	 * <em>Add Parameter Annotations</em>}' attribute
	 * @param value the new value of the '<em>Add Parameter Annotations</em>' attribute
	 * @see #isAddParameterAnnotations()
	 * @generated
	 */
	void setAddParameterAnnotations(boolean value);

	/**
	 * Return the value of the '<em><b>Return Value Name</b></em>' attribute
	 * @return the value of the '<em>Return Value Name</em>' attribute
	 * @see #setReturnValueName(String)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getSOAPIntegrationMethod_ReturnValueName()
	 * @model
	 * @generated
	 */
	String getReturnValueName();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.integration.SOAPIntegrationMethod#getReturnValueName <em>Return
	 * Value Name</em>}' attribute
	 * @param value the new value of the '<em>Return Value Name</em>' attribute
	 * @see #getReturnValueName()
	 * @generated
	 */
	void setReturnValueName(String value);

	/**
	 * Return the value of the '<em><b>Return Value Part Name</b></em>' attribute
	 * @return the value of the '<em>Return Value Part Name</em>' attribute
	 * @see #setReturnValuePartName(String)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getSOAPIntegrationMethod_ReturnValuePartName()
	 * @model
	 * @generated
	 */
	String getReturnValuePartName();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.integration.SOAPIntegrationMethod#getReturnValuePartName
	 * <em>Return Value Part Name</em>}' attribute
	 * @param value the new value of the '<em>Return Value Part Name</em>' attribute
	 * @see #getReturnValuePartName()
	 * @generated
	 */
	void setReturnValuePartName(String value);

}
