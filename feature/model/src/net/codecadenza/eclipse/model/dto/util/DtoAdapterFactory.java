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
package net.codecadenza.eclipse.model.dto.util;

import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.dto.DtoPackage;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.mapping.MappingAttribute;
import net.codecadenza.eclipse.model.mapping.MappingObject;
import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.emf.common.notify.impl.AdapterFactoryImpl;
import org.eclipse.emf.ecore.EObject;

/**
 * The <b>Adapter Factory</b> for the model. It provides an adapter <code>createXXX</code> method for each class of the model.
 * @see net.codecadenza.eclipse.model.dto.DtoPackage
 * @generated
 */
public class DtoAdapterFactory extends AdapterFactoryImpl {
	/**
	 * The cached model package
	 * @generated
	 */
	protected static DtoPackage modelPackage;

	/**
	 * Create an instance of the adapter factory
	 * @generated
	 */
	public DtoAdapterFactory() {
		if (modelPackage == null)
			modelPackage = DtoPackage.eINSTANCE;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.common.notify.impl.AdapterFactoryImpl#isFactoryForType(java.lang.Object)
	 * @generated
	 */
	@Override
	public boolean isFactoryForType(Object object) {
		if (object == modelPackage)
			return true;

		if (object instanceof final EObject eObject)
			return eObject.eClass().getEPackage() == modelPackage;

		return false;
	}

	/**
	 * The switch that delegates to the <code>createXXX</code> methods.
	 * @generated
	 */
	protected DtoSwitch<Adapter> modelSwitch = new DtoSwitch<>() {
		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.dto.util.DtoSwitch#caseDTOBean(net.codecadenza.eclipse.model.dto.DTOBean)
		 */
		@Override
		public Adapter caseDTOBean(DTOBean object) {
			return createDTOBeanAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.dto.util.DtoSwitch#caseDTOBeanAttribute(net.codecadenza.eclipse.model.dto.
		 * DTOBeanAttribute)
		 */
		@Override
		public Adapter caseDTOBeanAttribute(DTOBeanAttribute object) {
			return createDTOBeanAttributeAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.dto.util.DtoSwitch#caseJavaType(net.codecadenza.eclipse.model.java.JavaType)
		 */
		@Override
		public Adapter caseJavaType(JavaType object) {
			return createJavaTypeAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.dto.util.DtoSwitch#caseMappingObject(net.codecadenza.eclipse.model.mapping.MappingObject)
		 */
		@Override
		public Adapter caseMappingObject(MappingObject object) {
			return createMappingObjectAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.dto.util.DtoSwitch#caseMappingAttribute(net.codecadenza.eclipse.model.mapping.
		 * MappingAttribute)
		 */
		@Override
		public Adapter caseMappingAttribute(MappingAttribute object) {
			return createMappingAttributeAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.dto.util.DtoSwitch#defaultCase(org.eclipse.emf.ecore.EObject)
		 */
		@Override
		public Adapter defaultCase(EObject object) {
			return createEObjectAdapter();
		}
	};

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.common.notify.impl.AdapterFactoryImpl#createAdapter(org.eclipse.emf.common.notify.Notifier)
	 * @generated
	 */
	@Override
	public Adapter createAdapter(Notifier target) {
		return modelSwitch.doSwitch((EObject) target);
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.dto.DTOBean <em>DTO Bean</em>}'. This
	 * default implementation returns null so that we can easily ignore cases; it's useful to ignore a case when inheritance will
	 * catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.dto.DTOBean
	 * @generated
	 */
	public Adapter createDTOBeanAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.dto.DTOBeanAttribute <em>DTO Bean
	 * Attribute</em>}'. This default implementation returns null so that we can easily ignore cases; it's useful to ignore a case
	 * when inheritance will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.dto.DTOBeanAttribute
	 * @generated
	 */
	public Adapter createDTOBeanAttributeAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.java.JavaType <em>Java Type</em>}'. This
	 * default implementation returns null so that we can easily ignore cases; it's useful to ignore a case when inheritance will
	 * catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.java.JavaType
	 * @generated
	 */
	public Adapter createJavaTypeAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.mapping.MappingObject <em>Object</em>}'.
	 * This default implementation returns null so that we can easily ignore cases; it's useful to ignore a case when inheritance
	 * will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.mapping.MappingObject
	 * @generated
	 */
	public Adapter createMappingObjectAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.mapping.MappingAttribute <em>Mapping
	 * Attribute</em>}'. This default implementation returns null so that we can easily ignore cases; it's useful to ignore a case
	 * when inheritance will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.mapping.MappingAttribute
	 * @generated
	 */
	public Adapter createMappingAttributeAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for the default case. This default implementation returns null.
	 * @return the new adapter
	 * @generated
	 */
	public Adapter createEObjectAdapter() {
		return null;
	}

}
