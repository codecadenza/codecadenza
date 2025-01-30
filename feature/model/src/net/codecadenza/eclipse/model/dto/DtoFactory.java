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
package net.codecadenza.eclipse.model.dto;

import org.eclipse.emf.ecore.EFactory;

/**
 * The factory for meta-model objects of package <b>DTO</b>. It provides a create method for each non-abstract class of the model.
 * @see net.codecadenza.eclipse.model.dto.DtoPackage
 * @generated
 */
public interface DtoFactory extends EFactory {
	/**
	 * The singleton instance of the factory
	 * @generated
	 */
	DtoFactory eINSTANCE = net.codecadenza.eclipse.model.dto.impl.DtoFactoryImpl.init();

	/**
	 * @return a new {@link DTOBean} object
	 * @generated
	 */
	DTOBean createDTOBean();

	/**
	 * @return a new {@link DTOBeanAttribute} object
	 * @generated
	 */
	DTOBeanAttribute createDTOBeanAttribute();

	/**
	 * @return the package supported by this factory
	 * @generated
	 */
	DtoPackage getDtoPackage();

}
