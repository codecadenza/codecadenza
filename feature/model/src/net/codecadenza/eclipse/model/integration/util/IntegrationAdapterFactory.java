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
package net.codecadenza.eclipse.model.integration.util;

import net.codecadenza.eclipse.model.integration.AbstractIntegrationBean;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod;
import net.codecadenza.eclipse.model.integration.IntegrationPackage;
import net.codecadenza.eclipse.model.integration.JMSIntegrationBean;
import net.codecadenza.eclipse.model.integration.JMSIntegrationMethod;
import net.codecadenza.eclipse.model.integration.JMSResource;
import net.codecadenza.eclipse.model.integration.KafkaIntegrationBean;
import net.codecadenza.eclipse.model.integration.KafkaIntegrationMethod;
import net.codecadenza.eclipse.model.integration.RESTIntegrationBean;
import net.codecadenza.eclipse.model.integration.RESTIntegrationMethod;
import net.codecadenza.eclipse.model.integration.RMIIntegrationBean;
import net.codecadenza.eclipse.model.integration.RMIIntegrationMethod;
import net.codecadenza.eclipse.model.integration.SOAPIntegrationBean;
import net.codecadenza.eclipse.model.integration.SOAPIntegrationMethod;
import net.codecadenza.eclipse.model.java.JavaMethod;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.service.ServiceBean;
import net.codecadenza.eclipse.model.service.ServiceMethod;
import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.emf.common.notify.impl.AdapterFactoryImpl;
import org.eclipse.emf.ecore.EObject;

/**
 * The <b>Adapter Factory</b> for the model. It provides an adapter <code>createXXX</code> method for each class of the model.
 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage
 * @generated
 */
public class IntegrationAdapterFactory extends AdapterFactoryImpl {
	/**
	 * The cached model package
	 * @generated
	 */
	protected static IntegrationPackage modelPackage;

	/**
	 * Create an instance of the adapter factory
	 * @generated
	 */
	public IntegrationAdapterFactory() {
		if (modelPackage == null)
			modelPackage = IntegrationPackage.eINSTANCE;
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
	protected IntegrationSwitch<Adapter> modelSwitch = new IntegrationSwitch<>() {
		/*
		 * (non-Javadoc)
		 * @see
		 * net.codecadenza.eclipse.model.integration.util.IntegrationSwitch#caseSOAPIntegrationBean(net.codecadenza.eclipse.model.
		 * integration.SOAPIntegrationBean)
		 */
		@Override
		public Adapter caseSOAPIntegrationBean(SOAPIntegrationBean object) {
			return createSOAPIntegrationBeanAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see
		 * net.codecadenza.eclipse.model.integration.util.IntegrationSwitch#caseRESTIntegrationBean(net.codecadenza.eclipse.model.
		 * integration.RESTIntegrationBean)
		 */
		@Override
		public Adapter caseRESTIntegrationBean(RESTIntegrationBean object) {
			return createRESTIntegrationBeanAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.integration.util.IntegrationSwitch#caseAbstractIntegrationBean(net.codecadenza.eclipse.
		 * model.integration.AbstractIntegrationBean)
		 */
		@Override
		public Adapter caseAbstractIntegrationBean(AbstractIntegrationBean object) {
			return createAbstractIntegrationBeanAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.integration.util.IntegrationSwitch#caseSOAPIntegrationMethod(net.codecadenza.eclipse.
		 * model.integration.SOAPIntegrationMethod)
		 */
		@Override
		public Adapter caseSOAPIntegrationMethod(SOAPIntegrationMethod object) {
			return createSOAPIntegrationMethodAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.integration.util.IntegrationSwitch#caseRESTIntegrationMethod(net.codecadenza.eclipse.
		 * model.integration.RESTIntegrationMethod)
		 */
		@Override
		public Adapter caseRESTIntegrationMethod(RESTIntegrationMethod object) {
			return createRESTIntegrationMethodAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see
		 * net.codecadenza.eclipse.model.integration.util.IntegrationSwitch#caseAbstractIntegrationMethod(net.codecadenza.eclipse.
		 * model.integration.AbstractIntegrationMethod)
		 */
		@Override
		public Adapter caseAbstractIntegrationMethod(AbstractIntegrationMethod object) {
			return createAbstractIntegrationMethodAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see
		 * net.codecadenza.eclipse.model.integration.util.IntegrationSwitch#caseRMIIntegrationMethod(net.codecadenza.eclipse.model.
		 * integration.RMIIntegrationMethod)
		 */
		@Override
		public Adapter caseRMIIntegrationMethod(RMIIntegrationMethod object) {
			return createRMIIntegrationMethodAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.integration.util.IntegrationSwitch#caseRMIIntegrationBean(net.codecadenza.eclipse.model.
		 * integration.RMIIntegrationBean)
		 */
		@Override
		public Adapter caseRMIIntegrationBean(RMIIntegrationBean object) {
			return createRMIIntegrationBeanAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see
		 * net.codecadenza.eclipse.model.integration.util.IntegrationSwitch#caseKafkaIntegrationBean(net.codecadenza.eclipse.model.
		 * integration.KafkaIntegrationBean)
		 */
		@Override
		public Adapter caseKafkaIntegrationBean(KafkaIntegrationBean object) {
			return createKafkaIntegrationBeanAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.integration.util.IntegrationSwitch#caseKafkaIntegrationMethod(net.codecadenza.eclipse.
		 * model.integration.KafkaIntegrationMethod)
		 */
		@Override
		public Adapter caseKafkaIntegrationMethod(KafkaIntegrationMethod object) {
			return createKafkaIntegrationMethodAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see
		 * net.codecadenza.eclipse.model.integration.util.IntegrationSwitch#caseJMSIntegrationMethod(net.codecadenza.eclipse.model.
		 * integration.JMSIntegrationMethod)
		 */
		@Override
		public Adapter caseJMSIntegrationMethod(JMSIntegrationMethod object) {
			return createJMSIntegrationMethodAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.integration.util.IntegrationSwitch#caseJMSIntegrationBean(net.codecadenza.eclipse.model.
		 * integration.JMSIntegrationBean)
		 */
		@Override
		public Adapter caseJMSIntegrationBean(JMSIntegrationBean object) {
			return createJMSIntegrationBeanAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.integration.util.IntegrationSwitch#
		 * caseJMSResource(net.codecadenza.eclipse.model.integration.JMSResource)
		 */
		@Override
		public Adapter caseJMSResource(JMSResource object) {
			return createJMSResourceAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.integration.util.IntegrationSwitch#caseJavaType(net.codecadenza.eclipse.model.java.
		 * JavaType)
		 */
		@Override
		public Adapter caseJavaType(JavaType object) {
			return createJavaTypeAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see
		 * net.codecadenza.eclipse.model.integration.util.IntegrationSwitch#caseServiceBean(net.codecadenza.eclipse.model.service.
		 * ServiceBean)
		 */
		@Override
		public Adapter caseServiceBean(ServiceBean object) {
			return createServiceBeanAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.integration.util.IntegrationSwitch#caseJavaMethod(net.codecadenza.eclipse.model.java.
		 * JavaMethod)
		 */
		@Override
		public Adapter caseJavaMethod(JavaMethod object) {
			return createJavaMethodAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.integration.util.IntegrationSwitch#caseServiceMethod(net.codecadenza.eclipse.model.
		 * service.ServiceMethod)
		 */
		@Override
		public Adapter caseServiceMethod(ServiceMethod object) {
			return createServiceMethodAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.integration.util.IntegrationSwitch#defaultCase(org.eclipse.emf.ecore.EObject)
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
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.integration.SOAPIntegrationBean <em>SOAP
	 * Integration Bean</em>}'. This default implementation returns null so that we can easily ignore cases; it's useful to ignore a
	 * case when inheritance will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.integration.SOAPIntegrationBean
	 * @generated
	 */
	public Adapter createSOAPIntegrationBeanAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.integration.RESTIntegrationBean <em>REST
	 * Integration Bean</em>}'. This default implementation returns null so that we can easily ignore cases; it's useful to ignore a
	 * case when inheritance will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.integration.RESTIntegrationBean
	 * @generated
	 */
	public Adapter createRESTIntegrationBeanAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.integration.AbstractIntegrationBean
	 * <em>Abstract Integration Bean</em>}'. This default implementation returns null so that we can easily ignore cases; it's
	 * useful to ignore a case when inheritance will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.integration.AbstractIntegrationBean
	 * @generated
	 */
	public Adapter createAbstractIntegrationBeanAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.integration.SOAPIntegrationMethod <em>SOAP
	 * Integration Method</em>}'. This default implementation returns null so that we can easily ignore cases; it's useful to ignore
	 * a case when inheritance will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.integration.SOAPIntegrationMethod
	 * @generated
	 */
	public Adapter createSOAPIntegrationMethodAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.integration.RESTIntegrationMethod <em>REST
	 * Integration Method</em>}'. This default implementation returns null so that we can easily ignore cases; it's useful to ignore
	 * a case when inheritance will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.integration.RESTIntegrationMethod
	 * @generated
	 */
	public Adapter createRESTIntegrationMethodAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod
	 * <em>Abstract Integration Method</em>}'. This default implementation returns null so that we can easily ignore cases; it's
	 * useful to ignore a case when inheritance will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod
	 * @generated
	 */
	public Adapter createAbstractIntegrationMethodAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.integration.RMIIntegrationMethod <em>RMI
	 * Integration Method</em>}'. This default implementation returns null so that we can easily ignore cases; it's useful to ignore
	 * a case when inheritance will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.integration.RMIIntegrationMethod
	 * @generated
	 */
	public Adapter createRMIIntegrationMethodAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.integration.RMIIntegrationBean <em>RMI
	 * Integration Bean</em>}'. This default implementation returns null so that we can easily ignore cases; it's useful to ignore a
	 * case when inheritance will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.integration.RMIIntegrationBean
	 * @generated
	 */
	public Adapter createRMIIntegrationBeanAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.integration.KafkaIntegrationBean <em>Kafka
	 * Integration Bean</em>}'. This default implementation returns null so that we can easily ignore cases; it's useful to ignore a
	 * case when inheritance will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.integration.KafkaIntegrationBean
	 * @generated
	 */
	public Adapter createKafkaIntegrationBeanAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.integration.KafkaIntegrationMethod
	 * <em>Kafka Integration Method</em>}'. This default implementation returns null so that we can easily ignore cases; it's useful
	 * to ignore a case when inheritance will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.integration.KafkaIntegrationMethod
	 * @generated
	 */
	public Adapter createKafkaIntegrationMethodAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.integration.JMSIntegrationMethod <em>JMS
	 * Integration Method</em>}'. This default implementation returns null so that we can easily ignore cases; it's useful to ignore
	 * a case when inheritance will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.integration.JMSIntegrationMethod
	 * @generated
	 */
	public Adapter createJMSIntegrationMethodAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.integration.JMSIntegrationBean <em>JMS
	 * Integration Bean</em>}'. This default implementation returns null so that we can easily ignore cases; it's useful to ignore a
	 * case when inheritance will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.integration.JMSIntegrationBean
	 * @generated
	 */
	public Adapter createJMSIntegrationBeanAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.integration.JMSResource <em>JMS
	 * Resource</em>}'. This default implementation returns null so that we can easily ignore cases; it's useful to ignore a case
	 * when inheritance will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.integration.JMSResource
	 * @generated
	 */
	public Adapter createJMSResourceAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.java.JavaType <em>Type</em>}'. This default
	 * implementation returns null so that we can easily ignore cases; it's useful to ignore a case when inheritance will catch all
	 * the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.java.JavaType
	 * @generated
	 */
	public Adapter createJavaTypeAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.service.ServiceBean <em>Bean</em>}'. This
	 * default implementation returns null so that we can easily ignore cases; it's useful to ignore a case when inheritance will
	 * catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.service.ServiceBean
	 * @generated
	 */
	public Adapter createServiceBeanAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.java.JavaMethod <em>Method</em>}'. This
	 * default implementation returns null so that we can easily ignore cases; it's useful to ignore a case when inheritance will
	 * catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.java.JavaMethod
	 * @generated
	 */
	public Adapter createJavaMethodAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.service.ServiceMethod <em>Method</em>}'.
	 * This default implementation returns null so that we can easily ignore cases; it's useful to ignore a case when inheritance
	 * will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.service.ServiceMethod
	 * @generated
	 */
	public Adapter createServiceMethodAdapter() {
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
