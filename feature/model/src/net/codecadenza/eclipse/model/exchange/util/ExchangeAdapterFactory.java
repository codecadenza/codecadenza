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
package net.codecadenza.eclipse.model.exchange.util;

import net.codecadenza.eclipse.model.exchange.AssociationController;
import net.codecadenza.eclipse.model.exchange.DataExchangeAttribute;
import net.codecadenza.eclipse.model.exchange.DataExchangeElement;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;
import net.codecadenza.eclipse.model.exchange.DataExchangeMode;
import net.codecadenza.eclipse.model.exchange.DataExchangeServiceBean;
import net.codecadenza.eclipse.model.exchange.DirectExchangeMode;
import net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute;
import net.codecadenza.eclipse.model.exchange.ExchangeMappingObject;
import net.codecadenza.eclipse.model.exchange.ExchangePackage;
import net.codecadenza.eclipse.model.exchange.FileExchangeMode;
import net.codecadenza.eclipse.model.exchange.FilterMethodParameter;
import net.codecadenza.eclipse.model.exchange.StringExchangeMode;
import net.codecadenza.eclipse.model.exchange.ValueListEntry;
import net.codecadenza.eclipse.model.java.JavaMethod;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.MethodParameter;
import net.codecadenza.eclipse.model.mapping.MappingAttribute;
import net.codecadenza.eclipse.model.mapping.MappingObject;
import net.codecadenza.eclipse.model.service.ServiceBean;
import net.codecadenza.eclipse.model.service.ServiceMethod;
import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.emf.common.notify.impl.AdapterFactoryImpl;
import org.eclipse.emf.ecore.EObject;

/**
 * The <b>Adapter Factory</b> for the model. It provides an adapter <code>createXXX</code> method for each class of the model.
 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage
 * @generated
 */
public class ExchangeAdapterFactory extends AdapterFactoryImpl {
	/**
	 * The cached model package
	 * @generated
	 */
	protected static ExchangePackage modelPackage;

	/**
	 * Create an instance of the adapter factory
	 * @generated
	 */
	public ExchangeAdapterFactory() {
		if (modelPackage == null)
			modelPackage = ExchangePackage.eINSTANCE;
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
	protected ExchangeSwitch<Adapter> modelSwitch = new ExchangeSwitch<>() {
		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.exchange.util.ExchangeSwitch#caseDataExchangeServiceBean(net.codecadenza.eclipse.model.
		 * exchange.DataExchangeServiceBean)
		 */
		@Override
		public Adapter caseDataExchangeServiceBean(DataExchangeServiceBean object) {
			return createDataExchangeServiceBeanAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.exchange.util.ExchangeSwitch#caseDataExchangeMethod(net.codecadenza.eclipse.model.
		 * exchange.DataExchangeMethod)
		 */
		@Override
		public Adapter caseDataExchangeMethod(DataExchangeMethod object) {
			return createDataExchangeMethodAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.exchange.util.ExchangeSwitch#caseDataExchangeMode(net.codecadenza.eclipse.model.
		 * exchange.DataExchangeMode)
		 */
		@Override
		public Adapter caseDataExchangeMode(DataExchangeMode object) {
			return createDataExchangeModeAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.exchange.util.ExchangeSwitch#caseStringExchangeMode(net.codecadenza.eclipse.model.
		 * exchange.StringExchangeMode)
		 */
		@Override
		public Adapter caseStringExchangeMode(StringExchangeMode object) {
			return createStringExchangeModeAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.exchange.util.ExchangeSwitch#caseFileExchangeMode(net.codecadenza.eclipse.model.
		 * exchange.FileExchangeMode)
		 */
		@Override
		public Adapter caseFileExchangeMode(FileExchangeMode object) {
			return createFileExchangeModeAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.exchange.util.ExchangeSwitch#caseDirectExchangeMode(net.codecadenza.eclipse.model.
		 * exchange.DirectExchangeMode)
		 */
		@Override
		public Adapter caseDirectExchangeMode(DirectExchangeMode object) {
			return createDirectExchangeModeAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.exchange.util.ExchangeSwitch#
		 * caseDataExchangeElement(net.codecadenza.eclipse.model.exchange.DataExchangeElement)
		 */
		@Override
		public Adapter caseDataExchangeElement(DataExchangeElement object) {
			return createDataExchangeElementAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.exchange.util.ExchangeSwitch#caseDataExchangeAttribute(net.codecadenza.eclipse.model.
		 * exchange.DataExchangeAttribute)
		 */
		@Override
		public Adapter caseDataExchangeAttribute(DataExchangeAttribute object) {
			return createDataExchangeAttributeAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.exchange.util.ExchangeSwitch#caseValueListEntry(net.codecadenza.eclipse.model.exchange.
		 * ValueListEntry)
		 */
		@Override
		public Adapter caseValueListEntry(ValueListEntry object) {
			return createValueListEntryAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.exchange.util.ExchangeSwitch#caseExchangeMappingObject(net.codecadenza.eclipse.model.
		 * exchange.ExchangeMappingObject)
		 */
		@Override
		public Adapter caseExchangeMappingObject(ExchangeMappingObject object) {
			return createExchangeMappingObjectAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.exchange.util.ExchangeSwitch#caseExchangeMappingAttribute(net.codecadenza.eclipse.model.
		 * exchange.ExchangeMappingAttribute)
		 */
		@Override
		public Adapter caseExchangeMappingAttribute(ExchangeMappingAttribute object) {
			return createExchangeMappingAttributeAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.exchange.util.ExchangeSwitch#caseFilterMethodParameter(net.codecadenza.eclipse.model.
		 * exchange.FilterMethodParameter)
		 */
		@Override
		public Adapter caseFilterMethodParameter(FilterMethodParameter object) {
			return createFilterMethodParameterAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.exchange.util.ExchangeSwitch#caseAssociationController(net.codecadenza.eclipse.model.
		 * exchange.AssociationController)
		 */
		@Override
		public Adapter caseAssociationController(AssociationController object) {
			return createAssociationControllerAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.exchange.util.ExchangeSwitch#caseJavaType(net.codecadenza.eclipse.model.java.JavaType)
		 */
		@Override
		public Adapter caseJavaType(JavaType object) {
			return createJavaTypeAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.exchange.util.ExchangeSwitch#caseServiceBean(net.codecadenza.eclipse.model.service.
		 * ServiceBean)
		 */
		@Override
		public Adapter caseServiceBean(ServiceBean object) {
			return createServiceBeanAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.exchange.util.ExchangeSwitch#caseJavaMethod(net.codecadenza.eclipse.model.java.
		 * JavaMethod)
		 */
		@Override
		public Adapter caseJavaMethod(JavaMethod object) {
			return createJavaMethodAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.exchange.util.ExchangeSwitch#caseServiceMethod(net.codecadenza.eclipse.model.service.
		 * ServiceMethod)
		 */
		@Override
		public Adapter caseServiceMethod(ServiceMethod object) {
			return createServiceMethodAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.exchange.util.ExchangeSwitch#caseMappingObject(net.codecadenza.eclipse.model.mapping.
		 * MappingObject)
		 */
		@Override
		public Adapter caseMappingObject(MappingObject object) {
			return createMappingObjectAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.exchange.util.ExchangeSwitch#caseMappingAttribute(net.codecadenza.eclipse.model.mapping.
		 * MappingAttribute)
		 */
		@Override
		public Adapter caseMappingAttribute(MappingAttribute object) {
			return createMappingAttributeAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.exchange.util.ExchangeSwitch#caseMethodParameter(net.codecadenza.eclipse.model.java.
		 * MethodParameter)
		 */
		@Override
		public Adapter caseMethodParameter(MethodParameter object) {
			return createMethodParameterAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.exchange.util.ExchangeSwitch#defaultCase(org.eclipse.emf.ecore.EObject)
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
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.exchange.DataExchangeServiceBean <em>Data
	 * Exchange Service Bean</em>}'. This default implementation returns null so that we can easily ignore cases; it's useful to
	 * ignore a case when inheritance will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeServiceBean
	 * @generated
	 */
	public Adapter createDataExchangeServiceBeanAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod <em>Data
	 * Exchange Method</em>}'. This default implementation returns null so that we can easily ignore cases; it's useful to ignore a
	 * case when inheritance will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod
	 * @generated
	 */
	public Adapter createDataExchangeMethodAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.exchange.DataExchangeMode <em>Data Exchange
	 * Mode</em>}'. This default implementation returns null so that we can easily ignore cases; it's useful to ignore a case when
	 * inheritance will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMode
	 * @generated
	 */
	public Adapter createDataExchangeModeAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.exchange.StringExchangeMode <em>String
	 * Exchange Mode</em>}'. This default implementation returns null so that we can easily ignore cases; it's useful to ignore a
	 * case when inheritance will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.exchange.StringExchangeMode
	 * @generated
	 */
	public Adapter createStringExchangeModeAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.exchange.FileExchangeMode <em>File Exchange
	 * Mode</em>}'. This default implementation returns null so that we can easily ignore cases; it's useful to ignore a case when
	 * inheritance will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.exchange.FileExchangeMode
	 * @generated
	 */
	public Adapter createFileExchangeModeAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.exchange.DirectExchangeMode <em>Direct
	 * Exchange Mode</em>}'. This default implementation returns null so that we can easily ignore cases; it's useful to ignore a
	 * case when inheritance will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.exchange.DirectExchangeMode
	 * @generated
	 */
	public Adapter createDirectExchangeModeAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.exchange.DataExchangeElement <em>Data
	 * Exchange Element</em>}'. This default implementation returns null so that we can easily ignore cases; it's useful to ignore a
	 * case when inheritance will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeElement
	 * @generated
	 */
	public Adapter createDataExchangeElementAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.exchange.DataExchangeAttribute <em>Data
	 * Exchange Attribute</em>}'. This default implementation returns null so that we can easily ignore cases; it's useful to ignore
	 * a case when inheritance will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeAttribute
	 * @generated
	 */
	public Adapter createDataExchangeAttributeAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.exchange.ValueListEntry <em>Value List
	 * Entry</em>}'. This default implementation returns null so that we can easily ignore cases; it's useful to ignore a case when
	 * inheritance will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.exchange.ValueListEntry
	 * @generated
	 */
	public Adapter createValueListEntryAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.exchange.ExchangeMappingObject <em>Mapping
	 * Object</em>}'. This default implementation returns null so that we can easily ignore cases; it's useful to ignore a case when
	 * inheritance will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeMappingObject
	 * @generated
	 */
	public Adapter createExchangeMappingObjectAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute
	 * <em>Mapping Attribute</em>}'. This default implementation returns null so that we can easily ignore cases; it's useful to
	 * ignore a case when inheritance will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute
	 * @generated
	 */
	public Adapter createExchangeMappingAttributeAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.exchange.FilterMethodParameter <em>Filter
	 * Method Parameter</em>}'. This default implementation returns null so that we can easily ignore cases; it's useful to ignore a
	 * case when inheritance will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.exchange.FilterMethodParameter
	 * @generated
	 */
	public Adapter createFilterMethodParameterAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.exchange.AssociationController
	 * <em>Association Controller</em>}'. This default implementation returns null so that we can easily ignore cases; it's useful
	 * to ignore a case when inheritance will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.exchange.AssociationController
	 * @generated
	 */
	public Adapter createAssociationControllerAdapter() {
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
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.service.ServiceBean <em>Service
	 * Bean</em>}'. This default implementation returns null so that we can easily ignore cases; it's useful to ignore a case when
	 * inheritance will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.service.ServiceBean
	 * @generated
	 */
	public Adapter createServiceBeanAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.java.JavaMethod <em>Java Method</em>}'.
	 * This default implementation returns null so that we can easily ignore cases; it's useful to ignore a case when inheritance
	 * will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.java.JavaMethod
	 * @generated
	 */
	public Adapter createJavaMethodAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.service.ServiceMethod <em>Service
	 * Method</em>}'. This default implementation returns null so that we can easily ignore cases; it's useful to ignore a case when
	 * inheritance will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.service.ServiceMethod
	 * @generated
	 */
	public Adapter createServiceMethodAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.mapping.MappingObject <em>Mapping
	 * Object</em>}'. This default implementation returns null so that we can easily ignore cases; it's useful to ignore a case when
	 * inheritance will catch all the cases anyway.
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
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.java.MethodParameter <em>Method
	 * Parameter</em>}'. This default implementation returns null so that we can easily ignore cases; it's useful to ignore a case
	 * when inheritance will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.java.MethodParameter
	 * @generated
	 */
	public Adapter createMethodParameterAdapter() {
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
