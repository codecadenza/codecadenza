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

import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.service.ServiceBean;
import org.eclipse.emf.common.util.EList;

/**
 * A representation of the model object '<em><b>Data Exchange Service Bean</b></em>'.
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.exchange.DataExchangeServiceBean#getDataExchangeMethods <em>Data Exchange
 * Methods</em>}</li>
 * </ul>
 * </p>
 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeServiceBean()
 * @model
 * @generated
 */
public interface DataExchangeServiceBean extends ServiceBean {
	/**
	 * Return the value of the '<em><b>Data Exchange Methods</b></em>' containment reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod}. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getDataExchangeServiceBean <em>Data Exchange Service
	 * Bean</em>}'.
	 * @return the value of the '<em>Data Exchange Methods</em>' containment reference list
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeServiceBean_DataExchangeMethods()
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getDataExchangeServiceBean
	 * @model opposite="dataExchangeServiceBean" containment="true"
	 * @generated
	 */
	EList<DataExchangeMethod> getDataExchangeMethods();

	/**
	 * @return the internal representation of the data exchange bean source file
	 * @generated not
	 */
	JavaFile getSourceFile();

}
