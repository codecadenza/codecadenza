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
package net.codecadenza.eclipse.model.dto.impl;

import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.dto.DtoFactory;
import net.codecadenza.eclipse.model.dto.DtoPackage;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.impl.EFactoryImpl;
import org.eclipse.emf.ecore.plugin.EcorePlugin;

/**
 * An implementation of the model factory.
 * @generated
 */
public class DtoFactoryImpl extends EFactoryImpl implements DtoFactory {
	/**
	 * @return the default factory implementation
	 * @generated
	 */
	public static DtoFactory init() {
		try {
			final var theDtoFactory = (DtoFactory) EPackage.Registry.INSTANCE.getEFactory(DtoPackage.eNS_URI);

			if (theDtoFactory != null)
				return theDtoFactory;
		}
		catch (final Exception exception) {
			EcorePlugin.INSTANCE.log(exception);
		}

		return new DtoFactoryImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EFactoryImpl#create(org.eclipse.emf.ecore.EClass)
	 * @generated
	 */
	@Override
	public EObject create(EClass eClass) {
		return switch (eClass.getClassifierID()) {
			case DtoPackage.DTO_BEAN -> createDTOBean();
			case DtoPackage.DTO_BEAN_ATTRIBUTE -> createDTOBeanAttribute();
			default -> throw new IllegalArgumentException("The class '" + eClass.getName() + "' is not a valid classifier");
		};
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.dto.DtoFactory#createDTOBean()
	 * @generated
	 */
	@Override
	public DTOBean createDTOBean() {
		return new DTOBeanImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.dto.DtoFactory#createDTOBeanAttribute()
	 * @generated
	 */
	@Override
	public DTOBeanAttribute createDTOBeanAttribute() {
		return new DTOBeanAttributeImpl();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.dto.DtoFactory#getDtoPackage()
	 * @generated
	 */
	@Override
	public DtoPackage getDtoPackage() {
		return (DtoPackage) getEPackage();
	}

	/**
	 * @deprecated
	 * @return the DTO package
	 * @generated
	 */
	@Deprecated
	public static DtoPackage getPackage() {
		return DtoPackage.eINSTANCE;
	}

}
