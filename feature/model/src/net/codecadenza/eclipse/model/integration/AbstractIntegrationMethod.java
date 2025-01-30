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

import java.util.List;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.service.ServiceMethod;

/**
 * A representation of the model object '<em><b>Abstract Integration Method</b></em>'.
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod#getIntegrationBean <em>Integration
 * Bean</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod#getBoundaryMethod <em>Boundary Method</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod#isStartNewThread <em>Start New Thread</em>}</li>
 * </ul>
 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getAbstractIntegrationMethod()
 * @model
 * @generated
 */
public interface AbstractIntegrationMethod extends ServiceMethod {
	/**
	 * Return the value of the '<em><b>Integration Bean</b></em>' container reference. It is bidirectional and its opposite is '
	 * {@link net.codecadenza.eclipse.model.integration.AbstractIntegrationBean#getMethods <em>Methods</em>}'.
	 * @return the value of the '<em>Integration Bean</em>' container reference
	 * @see #setIntegrationBean(AbstractIntegrationBean)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getAbstractIntegrationMethod_IntegrationBean()
	 * @see net.codecadenza.eclipse.model.integration.AbstractIntegrationBean#getMethods
	 * @model opposite="methods" transient="false"
	 * @generated
	 */
	AbstractIntegrationBean getIntegrationBean();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod#getIntegrationBean
	 * <em>Integration Bean</em>}' container reference
	 * @param value the new value of the '<em>Integration Bean</em>' container reference
	 * @see #getIntegrationBean()
	 * @generated
	 */
	void setIntegrationBean(AbstractIntegrationBean value);

	/**
	 * Return the value of the '<em><b>Boundary Method</b></em>' reference
	 * @return the value of the '<em>Boundary Method</em>' reference
	 * @see #setBoundaryMethod(BoundaryMethod)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getAbstractIntegrationMethod_BoundaryMethod()
	 * @model
	 * @generated
	 */
	BoundaryMethod getBoundaryMethod();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod#getBoundaryMethod
	 * <em>Boundary Method</em>}' reference
	 * @param value the new value of the '<em>Boundary Method</em>' reference
	 * @see #getBoundaryMethod()
	 * @generated
	 */
	void setBoundaryMethod(BoundaryMethod value);

	/**
	 * Return the value of the '<em><b>Start New Thread</b></em>' attribute
	 * @return the value of the '<em>Start New Thread</em>' attribute
	 * @see #setStartNewThread(boolean)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getAbstractIntegrationMethod_StartNewThread()
	 * @model
	 * @generated
	 */
	boolean isStartNewThread();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod#isStartNewThread <em>Start
	 * New Thread</em>}' attribute
	 * @param value the new value of the '<em>Start New Thread</em>' attribute
	 * @see #isStartNewThread()
	 * @generated
	 */
	void setStartNewThread(boolean value);

	/**
	 * @return a list containing all integration parameters of this method
	 * @generated not
	 */
	List<IntegrationMethodParameter> getIntegrationParameters();

}
