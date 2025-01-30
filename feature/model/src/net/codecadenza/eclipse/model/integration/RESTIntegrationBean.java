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
 * A representation of the model object '<em><b>REST Integration Bean</b></em>'.
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.integration.RESTIntegrationBean#getPath <em>Path</em>}</li>
 * </ul>
 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getRESTIntegrationBean()
 * @model
 * @generated
 */
public interface RESTIntegrationBean extends AbstractIntegrationBean {
	/**
	 * Return the value of the '<em><b>Path</b></em>' attribute
	 * @return the value of the '<em>Path</em>' attribute
	 * @see #setPath(String)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getRESTIntegrationBean_Path()
	 * @model
	 * @generated
	 */
	String getPath();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.integration.RESTIntegrationBean#getPath <em>Path</em>}' attribute
	 * @param value the new value of the '<em>Path</em>' attribute
	 * @see #getPath()
	 * @generated
	 */
	void setPath(String value);

	/**
	 * Convert the given enumeration literal to a JAX-RS media type
	 * @param mediaType the enumeration literal to be converted
	 * @return the JAX-RS media type or null if the provided parameter is either not supported or {@link MediaTypeEnumeration#NONE}
	 * @generated not
	 */
	String convertToMediaType(MediaTypeEnumeration mediaType);

	/**
	 * @return the primary consumed media type
	 * @generated not
	 */
	String getPrimaryConsumedMediaType();

	/**
	 * @return the primary produced media type
	 * @generated not
	 */
	String getPrimaryProducedMediaType();

}
