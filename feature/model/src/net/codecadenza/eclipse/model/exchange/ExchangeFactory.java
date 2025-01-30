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

import org.eclipse.emf.ecore.EFactory;

/**
 * The factory for meta-model objects of package <b>Exchange</b>. It provides a create method for each non-abstract class of the
 * model.
 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage
 * @generated
 */
public interface ExchangeFactory extends EFactory {
	/**
	 * The singleton instance of the factory
	 * @generated
	 */
	ExchangeFactory eINSTANCE = net.codecadenza.eclipse.model.exchange.impl.ExchangeFactoryImpl.init();

	/**
	 * @return a new {@link DataExchangeServiceBean} object
	 * @generated
	 */
	DataExchangeServiceBean createDataExchangeServiceBean();

	/**
	 * @return a new {@link DataExchangeMethod} object
	 * @generated
	 */
	DataExchangeMethod createDataExchangeMethod();

	/**
	 * @return a new {@link StringExchangeMode} object
	 * @generated
	 */
	StringExchangeMode createStringExchangeMode();

	/**
	 * @return a new {@link FileExchangeMode} object
	 * @generated
	 */
	FileExchangeMode createFileExchangeMode();

	/**
	 * @return a new {@link DataExchangeElement} object
	 * @generated
	 */
	DataExchangeElement createDataExchangeElement();

	/**
	 * @return a new {@link DataExchangeAttribute} object
	 * @generated
	 */
	DataExchangeAttribute createDataExchangeAttribute();

	/**
	 * @return a new {@link ValueListEntry} object
	 * @generated
	 */
	ValueListEntry createValueListEntry();

	/**
	 * @return a new {@link ExchangeMappingObject} object
	 * @generated
	 */
	ExchangeMappingObject createExchangeMappingObject();

	/**
	 * @return a new {@link ExchangeMappingAttribute} object
	 * @generated
	 */
	ExchangeMappingAttribute createExchangeMappingAttribute();

	/**
	 * @return a new {@link FilterMethodParameter} object
	 * @generated
	 */
	FilterMethodParameter createFilterMethodParameter();

	/**
	 * @return a new {@link AssociationController} object
	 * @generated
	 */
	AssociationController createAssociationController();

	/**
	 * @return a new {@link DirectExchangeMode} object
	 * @generated
	 */
	DirectExchangeMode createDirectExchangeMode();

	/**
	 * @return the package supported by this factory
	 * @generated
	 */
	ExchangePackage getExchangePackage();

}
