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
package net.codecadenza.eclipse.model.integration.impl;

import net.codecadenza.eclipse.model.boundary.BoundaryPackage;
import net.codecadenza.eclipse.model.boundary.impl.BoundaryPackageImpl;
import net.codecadenza.eclipse.model.client.ClientPackage;
import net.codecadenza.eclipse.model.client.impl.ClientPackageImpl;
import net.codecadenza.eclipse.model.db.DbPackage;
import net.codecadenza.eclipse.model.db.impl.DbPackageImpl;
import net.codecadenza.eclipse.model.domain.DomainPackage;
import net.codecadenza.eclipse.model.domain.impl.DomainPackageImpl;
import net.codecadenza.eclipse.model.dto.DtoPackage;
import net.codecadenza.eclipse.model.dto.impl.DtoPackageImpl;
import net.codecadenza.eclipse.model.exchange.ExchangePackage;
import net.codecadenza.eclipse.model.exchange.impl.ExchangePackageImpl;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationBean;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod;
import net.codecadenza.eclipse.model.integration.HttpMethodEnumeration;
import net.codecadenza.eclipse.model.integration.IntegrationFactory;
import net.codecadenza.eclipse.model.integration.IntegrationPackage;
import net.codecadenza.eclipse.model.integration.JMSIntegrationBean;
import net.codecadenza.eclipse.model.integration.JMSIntegrationMethod;
import net.codecadenza.eclipse.model.integration.JMSResource;
import net.codecadenza.eclipse.model.integration.KafkaIntegrationBean;
import net.codecadenza.eclipse.model.integration.KafkaIntegrationMethod;
import net.codecadenza.eclipse.model.integration.MediaTypeEnumeration;
import net.codecadenza.eclipse.model.integration.RESTIntegrationBean;
import net.codecadenza.eclipse.model.integration.RESTIntegrationMethod;
import net.codecadenza.eclipse.model.integration.RMIIntegrationBean;
import net.codecadenza.eclipse.model.integration.RMIIntegrationMethod;
import net.codecadenza.eclipse.model.integration.SOAPIntegrationBean;
import net.codecadenza.eclipse.model.integration.SOAPIntegrationMethod;
import net.codecadenza.eclipse.model.java.JavaPackage;
import net.codecadenza.eclipse.model.java.impl.JavaPackageImpl;
import net.codecadenza.eclipse.model.mapping.MappingPackage;
import net.codecadenza.eclipse.model.mapping.impl.MappingPackageImpl;
import net.codecadenza.eclipse.model.project.ProjectPackage;
import net.codecadenza.eclipse.model.project.impl.ProjectPackageImpl;
import net.codecadenza.eclipse.model.repository.RepositoryPackage;
import net.codecadenza.eclipse.model.repository.impl.RepositoryPackageImpl;
import net.codecadenza.eclipse.model.service.ServicePackage;
import net.codecadenza.eclipse.model.service.impl.ServicePackageImpl;
import net.codecadenza.eclipse.model.testing.TestingPackage;
import net.codecadenza.eclipse.model.testing.impl.TestingPackageImpl;
import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EEnum;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.impl.EPackageImpl;

/**
 * An implementation of the model <b>Package</b>.
 * @generated
 */
public class IntegrationPackageImpl extends EPackageImpl implements IntegrationPackage {
	/**
	 * @generated
	 */
	private EClass soapIntegrationBeanEClass;

	/**
	 * @generated
	 */
	private EClass restIntegrationBeanEClass;

	/**
	 * @generated
	 */
	private EClass abstractIntegrationBeanEClass;

	/**
	 * @generated
	 */
	private EClass soapIntegrationMethodEClass;

	/**
	 * @generated
	 */
	private EClass restIntegrationMethodEClass;

	/**
	 * @generated
	 */
	private EClass abstractIntegrationMethodEClass;

	/**
	 * @generated
	 */
	private EClass rmiIntegrationMethodEClass;

	/**
	 * @generated
	 */
	private EClass rmiIntegrationBeanEClass;

	/**
	 * @generated
	 */
	private EClass kafkaIntegrationBeanEClass;

	/**
	 * @generated
	 */
	private EClass kafkaIntegrationMethodEClass;

	/**
	 * @generated
	 */
	private EClass jmsIntegrationMethodEClass;

	/**
	 * @generated
	 */
	private EClass jmsIntegrationBeanEClass;

	/**
	 * @generated
	 */
	private EClass jmsResourceEClass;

	/**
	 * @generated
	 */
	private EEnum mediaTypeEnumerationEEnum;

	/**
	 * @generated
	 */
	private EEnum httpMethodEnumerationEEnum;

	/**
	 * Create an instance of the model <b>Package</b>, registered with {@link org.eclipse.emf.ecore.EPackage.Registry
	 * EPackage.Registry} by the package package URI value.
	 * <p>
	 * Note: the correct way to create the package is via the static factory method {@link #init init()}, which also performs
	 * initialization of the package, or returns the registered package, if one already exists.
	 * @see org.eclipse.emf.ecore.EPackage.Registry
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#eNS_URI
	 * @see #init()
	 * @generated
	 */
	private IntegrationPackageImpl() {
		super(eNS_URI, IntegrationFactory.eINSTANCE);
	}

	/**
	 * @generated
	 */
	private static boolean isInited;

	/**
	 * Create, register and initialize the <b>Package</b> for this model, and for any others upon which it depends.
	 * <p>
	 * This method is used to initialize {@link IntegrationPackage#eINSTANCE} when that field is accessed. Clients should not invoke
	 * it directly. Instead, they should simply access that field to obtain the package.
	 * @see #eNS_URI
	 * @see #createPackageContents()
	 * @see #initializePackageContents()
	 * @return the initialized integration package
	 * @generated
	 */
	public static IntegrationPackage init() {
		if (isInited)
			return (IntegrationPackage) EPackage.Registry.INSTANCE.getEPackage(IntegrationPackage.eNS_URI);

		// Obtain or create and register package
		final var theIntegrationPackage = (IntegrationPackageImpl) (EPackage.Registry.INSTANCE
				.get(eNS_URI) instanceof IntegrationPackageImpl ? EPackage.Registry.INSTANCE.get(eNS_URI) : new IntegrationPackageImpl());

		isInited = true;

		// Obtain or create and register interdependencies
		final var theBoundaryPackage = (BoundaryPackageImpl) (EPackage.Registry.INSTANCE
				.getEPackage(BoundaryPackage.eNS_URI) instanceof BoundaryPackageImpl
						? EPackage.Registry.INSTANCE.getEPackage(BoundaryPackage.eNS_URI) : BoundaryPackage.eINSTANCE);
		final var theClientPackage = (ClientPackageImpl) (EPackage.Registry.INSTANCE
				.getEPackage(ClientPackage.eNS_URI) instanceof ClientPackageImpl
						? EPackage.Registry.INSTANCE.getEPackage(ClientPackage.eNS_URI) : ClientPackage.eINSTANCE);
		final var theRepositoryPackage = (RepositoryPackageImpl) (EPackage.Registry.INSTANCE
				.getEPackage(RepositoryPackage.eNS_URI) instanceof RepositoryPackageImpl
						? EPackage.Registry.INSTANCE.getEPackage(RepositoryPackage.eNS_URI) : RepositoryPackage.eINSTANCE);
		final var theDbPackage = (DbPackageImpl) (EPackage.Registry.INSTANCE.getEPackage(DbPackage.eNS_URI) instanceof DbPackageImpl
				? EPackage.Registry.INSTANCE.getEPackage(DbPackage.eNS_URI) : DbPackage.eINSTANCE);
		final var theDomainPackage = (DomainPackageImpl) (EPackage.Registry.INSTANCE
				.getEPackage(DomainPackage.eNS_URI) instanceof DomainPackageImpl
						? EPackage.Registry.INSTANCE.getEPackage(DomainPackage.eNS_URI) : DomainPackage.eINSTANCE);
		final var theDtoPackage = (DtoPackageImpl) (EPackage.Registry.INSTANCE
				.getEPackage(DtoPackage.eNS_URI) instanceof DtoPackageImpl ? EPackage.Registry.INSTANCE.getEPackage(DtoPackage.eNS_URI)
						: DtoPackage.eINSTANCE);
		final var theExchangePackage = (ExchangePackageImpl) (EPackage.Registry.INSTANCE
				.getEPackage(ExchangePackage.eNS_URI) instanceof ExchangePackageImpl
						? EPackage.Registry.INSTANCE.getEPackage(ExchangePackage.eNS_URI) : ExchangePackage.eINSTANCE);
		final var theJavaPackage = (JavaPackageImpl) (EPackage.Registry.INSTANCE
				.getEPackage(JavaPackage.eNS_URI) instanceof JavaPackageImpl ? EPackage.Registry.INSTANCE.getEPackage(JavaPackage.eNS_URI)
						: JavaPackage.eINSTANCE);
		final var theMappingPackage = (MappingPackageImpl) (EPackage.Registry.INSTANCE
				.getEPackage(MappingPackage.eNS_URI) instanceof MappingPackageImpl
						? EPackage.Registry.INSTANCE.getEPackage(MappingPackage.eNS_URI) : MappingPackage.eINSTANCE);
		final var theProjectPackage = (ProjectPackageImpl) (EPackage.Registry.INSTANCE
				.getEPackage(ProjectPackage.eNS_URI) instanceof ProjectPackageImpl
						? EPackage.Registry.INSTANCE.getEPackage(ProjectPackage.eNS_URI) : ProjectPackage.eINSTANCE);
		final var theServicePackage = (ServicePackageImpl) (EPackage.Registry.INSTANCE
				.getEPackage(ServicePackage.eNS_URI) instanceof ServicePackageImpl
						? EPackage.Registry.INSTANCE.getEPackage(ServicePackage.eNS_URI) : ServicePackage.eINSTANCE);
		final var theTestingPackage = (TestingPackageImpl) (EPackage.Registry.INSTANCE
				.getEPackage(TestingPackage.eNS_URI) instanceof TestingPackageImpl
						? EPackage.Registry.INSTANCE.getEPackage(TestingPackage.eNS_URI) : TestingPackage.eINSTANCE);

		// Create package meta-data objects
		theIntegrationPackage.createPackageContents();
		theBoundaryPackage.createPackageContents();
		theClientPackage.createPackageContents();
		theRepositoryPackage.createPackageContents();
		theDbPackage.createPackageContents();
		theDomainPackage.createPackageContents();
		theDtoPackage.createPackageContents();
		theExchangePackage.createPackageContents();
		theJavaPackage.createPackageContents();
		theMappingPackage.createPackageContents();
		theProjectPackage.createPackageContents();
		theServicePackage.createPackageContents();
		theTestingPackage.createPackageContents();

		// Initialize created meta-data
		theIntegrationPackage.initializePackageContents();
		theBoundaryPackage.initializePackageContents();
		theClientPackage.initializePackageContents();
		theRepositoryPackage.initializePackageContents();
		theDbPackage.initializePackageContents();
		theDomainPackage.initializePackageContents();
		theDtoPackage.initializePackageContents();
		theExchangePackage.initializePackageContents();
		theJavaPackage.initializePackageContents();
		theMappingPackage.initializePackageContents();
		theProjectPackage.initializePackageContents();
		theServicePackage.initializePackageContents();
		theTestingPackage.initializePackageContents();

		// Mark meta-data to indicate it can't be changed
		theIntegrationPackage.freeze();

		// Update the registry and return the package
		EPackage.Registry.INSTANCE.put(IntegrationPackage.eNS_URI, theIntegrationPackage);
		return theIntegrationPackage;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getSOAPIntegrationBean()
	 * @generated
	 */
	@Override
	public EClass getSOAPIntegrationBean() {
		return soapIntegrationBeanEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getSOAPIntegrationBean_ServiceName()
	 * @generated
	 */
	@Override
	public EAttribute getSOAPIntegrationBean_ServiceName() {
		return (EAttribute) soapIntegrationBeanEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getSOAPIntegrationBean_PortName()
	 * @generated
	 */
	@Override
	public EAttribute getSOAPIntegrationBean_PortName() {
		return (EAttribute) soapIntegrationBeanEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getSOAPIntegrationBean_RpcStype()
	 * @generated
	 */
	@Override
	public EAttribute getSOAPIntegrationBean_RpcStype() {
		return (EAttribute) soapIntegrationBeanEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getSOAPIntegrationBean_BareParameterStyle()
	 * @generated
	 */
	@Override
	public EAttribute getSOAPIntegrationBean_BareParameterStyle() {
		return (EAttribute) soapIntegrationBeanEClass.getEStructuralFeatures().get(3);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getSOAPIntegrationBean_PortTypeName()
	 * @generated
	 */
	@Override
	public EAttribute getSOAPIntegrationBean_PortTypeName() {
		return (EAttribute) soapIntegrationBeanEClass.getEStructuralFeatures().get(4);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getRESTIntegrationBean()
	 * @generated
	 */
	@Override
	public EClass getRESTIntegrationBean() {
		return restIntegrationBeanEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getRESTIntegrationBean_Path()
	 * @generated
	 */
	@Override
	public EAttribute getRESTIntegrationBean_Path() {
		return (EAttribute) restIntegrationBeanEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getAbstractIntegrationBean()
	 * @generated
	 */
	@Override
	public EClass getAbstractIntegrationBean() {
		return abstractIntegrationBeanEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getAbstractIntegrationBean_Methods()
	 * @generated
	 */
	@Override
	public EReference getAbstractIntegrationBean_Methods() {
		return (EReference) abstractIntegrationBeanEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getAbstractIntegrationBean_ClientClassName()
	 * @generated
	 */
	@Override
	public EAttribute getAbstractIntegrationBean_ClientClassName() {
		return (EAttribute) abstractIntegrationBeanEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getAbstractIntegrationBean_ProducerClassName()
	 * @generated
	 */
	@Override
	public EAttribute getAbstractIntegrationBean_ProducerClassName() {
		return (EAttribute) abstractIntegrationBeanEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getSOAPIntegrationMethod()
	 * @generated
	 */
	@Override
	public EClass getSOAPIntegrationMethod() {
		return soapIntegrationMethodEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getSOAPIntegrationMethod_OperationName()
	 * @generated
	 */
	@Override
	public EAttribute getSOAPIntegrationMethod_OperationName() {
		return (EAttribute) soapIntegrationMethodEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getSOAPIntegrationMethod_AddParameterAnnotations()
	 * @generated
	 */
	@Override
	public EAttribute getSOAPIntegrationMethod_AddParameterAnnotations() {
		return (EAttribute) soapIntegrationMethodEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getSOAPIntegrationMethod_ReturnValueName()
	 * @generated
	 */
	@Override
	public EAttribute getSOAPIntegrationMethod_ReturnValueName() {
		return (EAttribute) soapIntegrationMethodEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getSOAPIntegrationMethod_ReturnValuePartName()
	 * @generated
	 */
	@Override
	public EAttribute getSOAPIntegrationMethod_ReturnValuePartName() {
		return (EAttribute) soapIntegrationMethodEClass.getEStructuralFeatures().get(3);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getRESTIntegrationMethod()
	 * @generated
	 */
	@Override
	public EClass getRESTIntegrationMethod() {
		return restIntegrationMethodEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getRESTIntegrationMethod_Path()
	 * @generated
	 */
	@Override
	public EAttribute getRESTIntegrationMethod_Path() {
		return (EAttribute) restIntegrationMethodEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getRESTIntegrationMethod_HttpMethod()
	 * @generated
	 */
	@Override
	public EAttribute getRESTIntegrationMethod_HttpMethod() {
		return (EAttribute) restIntegrationMethodEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getRESTIntegrationMethod_InputType()
	 * @generated
	 */
	@Override
	public EAttribute getRESTIntegrationMethod_InputType() {
		return (EAttribute) restIntegrationMethodEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getRESTIntegrationMethod_OutputType()
	 * @generated
	 */
	@Override
	public EAttribute getRESTIntegrationMethod_OutputType() {
		return (EAttribute) restIntegrationMethodEClass.getEStructuralFeatures().get(3);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getAbstractIntegrationMethod()
	 * @generated
	 */
	@Override
	public EClass getAbstractIntegrationMethod() {
		return abstractIntegrationMethodEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getAbstractIntegrationMethod_IntegrationBean()
	 * @generated
	 */
	@Override
	public EReference getAbstractIntegrationMethod_IntegrationBean() {
		return (EReference) abstractIntegrationMethodEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getAbstractIntegrationMethod_BoundaryMethod()
	 * @generated
	 */
	@Override
	public EReference getAbstractIntegrationMethod_BoundaryMethod() {
		return (EReference) abstractIntegrationMethodEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getAbstractIntegrationMethod_StartNewThread()
	 * @generated
	 */
	@Override
	public EAttribute getAbstractIntegrationMethod_StartNewThread() {
		return (EAttribute) abstractIntegrationMethodEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getRMIIntegrationMethod()
	 * @generated
	 */
	@Override
	public EClass getRMIIntegrationMethod() {
		return rmiIntegrationMethodEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getRMIIntegrationBean()
	 * @generated
	 */
	@Override
	public EClass getRMIIntegrationBean() {
		return rmiIntegrationBeanEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getKafkaIntegrationBean()
	 * @generated
	 */
	@Override
	public EClass getKafkaIntegrationBean() {
		return kafkaIntegrationBeanEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getKafkaIntegrationBean_RequestTopic()
	 * @generated
	 */
	@Override
	public EAttribute getKafkaIntegrationBean_RequestTopic() {
		return (EAttribute) kafkaIntegrationBeanEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getKafkaIntegrationBean_ResponseTopic()
	 * @generated
	 */
	@Override
	public EAttribute getKafkaIntegrationBean_ResponseTopic() {
		return (EAttribute) kafkaIntegrationBeanEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getKafkaIntegrationBean_ConsumerGroup()
	 * @generated
	 */
	@Override
	public EAttribute getKafkaIntegrationBean_ConsumerGroup() {
		return (EAttribute) kafkaIntegrationBeanEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getKafkaIntegrationMethod()
	 * @generated
	 */
	@Override
	public EClass getKafkaIntegrationMethod() {
		return kafkaIntegrationMethodEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getKafkaIntegrationMethod_RequestSchemaName()
	 * @generated
	 */
	@Override
	public EAttribute getKafkaIntegrationMethod_RequestSchemaName() {
		return (EAttribute) kafkaIntegrationMethodEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getKafkaIntegrationMethod_ResponseSchemaName()
	 * @generated
	 */
	@Override
	public EAttribute getKafkaIntegrationMethod_ResponseSchemaName() {
		return (EAttribute) kafkaIntegrationMethodEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getKafkaIntegrationMethod_UseDedicatedPartition()
	 * @generated
	 */
	@Override
	public EAttribute getKafkaIntegrationMethod_UseDedicatedPartition() {
		return (EAttribute) kafkaIntegrationMethodEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getKafkaIntegrationMethod_SendResponse()
	 * @generated
	 */
	@Override
	public EAttribute getKafkaIntegrationMethod_SendResponse() {
		return (EAttribute) kafkaIntegrationMethodEClass.getEStructuralFeatures().get(3);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getJMSIntegrationMethod()
	 * @generated
	 */
	@Override
	public EClass getJMSIntegrationMethod() {
		return jmsIntegrationMethodEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getJMSIntegrationMethod_OperationID()
	 * @generated
	 */
	@Override
	public EAttribute getJMSIntegrationMethod_OperationID() {
		return (EAttribute) jmsIntegrationMethodEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getJMSIntegrationMethod_SendResponse()
	 * @generated
	 */
	@Override
	public EAttribute getJMSIntegrationMethod_SendResponse() {
		return (EAttribute) jmsIntegrationMethodEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getJMSIntegrationBean()
	 * @generated
	 */
	@Override
	public EClass getJMSIntegrationBean() {
		return jmsIntegrationBeanEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getJMSIntegrationBean_RequestDestination()
	 * @generated
	 */
	@Override
	public EReference getJMSIntegrationBean_RequestDestination() {
		return (EReference) jmsIntegrationBeanEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getJMSIntegrationBean_ResponseDestination()
	 * @generated
	 */
	@Override
	public EReference getJMSIntegrationBean_ResponseDestination() {
		return (EReference) jmsIntegrationBeanEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getJMSResource()
	 * @generated
	 */
	@Override
	public EClass getJMSResource() {
		return jmsResourceEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getJMSResource_Name()
	 * @generated
	 */
	@Override
	public EAttribute getJMSResource_Name() {
		return (EAttribute) jmsResourceEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getJMSResource_Topic()
	 * @generated
	 */
	@Override
	public EAttribute getJMSResource_Topic() {
		return (EAttribute) jmsResourceEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getMediaTypeEnumeration()
	 * @generated
	 */
	@Override
	public EEnum getMediaTypeEnumeration() {
		return mediaTypeEnumerationEEnum;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getHttpMethodEnumeration()
	 * @generated
	 */
	@Override
	public EEnum getHttpMethodEnumeration() {
		return httpMethodEnumerationEEnum;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.integration.IntegrationPackage#getIntegrationFactory()
	 * @generated
	 */
	@Override
	public IntegrationFactory getIntegrationFactory() {
		return (IntegrationFactory) getEFactoryInstance();
	}

	/**
	 * @generated
	 */
	private boolean isCreated;

	/**
	 * Create the meta-model objects for the package. This method is guarded to have no affect on any invocation but its first.
	 * @generated
	 */
	public void createPackageContents() {
		if (isCreated)
			return;

		isCreated = true;

		// Create classes and their features
		soapIntegrationBeanEClass = createEClass(SOAP_INTEGRATION_BEAN);
		createEAttribute(soapIntegrationBeanEClass, SOAP_INTEGRATION_BEAN__SERVICE_NAME);
		createEAttribute(soapIntegrationBeanEClass, SOAP_INTEGRATION_BEAN__PORT_NAME);
		createEAttribute(soapIntegrationBeanEClass, SOAP_INTEGRATION_BEAN__RPC_STYPE);
		createEAttribute(soapIntegrationBeanEClass, SOAP_INTEGRATION_BEAN__BARE_PARAMETER_STYLE);
		createEAttribute(soapIntegrationBeanEClass, SOAP_INTEGRATION_BEAN__PORT_TYPE_NAME);

		restIntegrationBeanEClass = createEClass(REST_INTEGRATION_BEAN);
		createEAttribute(restIntegrationBeanEClass, REST_INTEGRATION_BEAN__PATH);

		abstractIntegrationBeanEClass = createEClass(ABSTRACT_INTEGRATION_BEAN);
		createEReference(abstractIntegrationBeanEClass, ABSTRACT_INTEGRATION_BEAN__METHODS);
		createEAttribute(abstractIntegrationBeanEClass, ABSTRACT_INTEGRATION_BEAN__CLIENT_CLASS_NAME);
		createEAttribute(abstractIntegrationBeanEClass, ABSTRACT_INTEGRATION_BEAN__PRODUCER_CLASS_NAME);

		soapIntegrationMethodEClass = createEClass(SOAP_INTEGRATION_METHOD);
		createEAttribute(soapIntegrationMethodEClass, SOAP_INTEGRATION_METHOD__OPERATION_NAME);
		createEAttribute(soapIntegrationMethodEClass, SOAP_INTEGRATION_METHOD__ADD_PARAMETER_ANNOTATIONS);
		createEAttribute(soapIntegrationMethodEClass, SOAP_INTEGRATION_METHOD__RETURN_VALUE_NAME);
		createEAttribute(soapIntegrationMethodEClass, SOAP_INTEGRATION_METHOD__RETURN_VALUE_PART_NAME);

		restIntegrationMethodEClass = createEClass(REST_INTEGRATION_METHOD);
		createEAttribute(restIntegrationMethodEClass, REST_INTEGRATION_METHOD__PATH);
		createEAttribute(restIntegrationMethodEClass, REST_INTEGRATION_METHOD__HTTP_METHOD);
		createEAttribute(restIntegrationMethodEClass, REST_INTEGRATION_METHOD__INPUT_TYPE);
		createEAttribute(restIntegrationMethodEClass, REST_INTEGRATION_METHOD__OUTPUT_TYPE);

		abstractIntegrationMethodEClass = createEClass(ABSTRACT_INTEGRATION_METHOD);
		createEReference(abstractIntegrationMethodEClass, ABSTRACT_INTEGRATION_METHOD__INTEGRATION_BEAN);
		createEReference(abstractIntegrationMethodEClass, ABSTRACT_INTEGRATION_METHOD__BOUNDARY_METHOD);
		createEAttribute(abstractIntegrationMethodEClass, ABSTRACT_INTEGRATION_METHOD__START_NEW_THREAD);

		rmiIntegrationMethodEClass = createEClass(RMI_INTEGRATION_METHOD);

		rmiIntegrationBeanEClass = createEClass(RMI_INTEGRATION_BEAN);

		kafkaIntegrationBeanEClass = createEClass(KAFKA_INTEGRATION_BEAN);
		createEAttribute(kafkaIntegrationBeanEClass, KAFKA_INTEGRATION_BEAN__REQUEST_TOPIC);
		createEAttribute(kafkaIntegrationBeanEClass, KAFKA_INTEGRATION_BEAN__RESPONSE_TOPIC);
		createEAttribute(kafkaIntegrationBeanEClass, KAFKA_INTEGRATION_BEAN__CONSUMER_GROUP);

		kafkaIntegrationMethodEClass = createEClass(KAFKA_INTEGRATION_METHOD);
		createEAttribute(kafkaIntegrationMethodEClass, KAFKA_INTEGRATION_METHOD__REQUEST_SCHEMA_NAME);
		createEAttribute(kafkaIntegrationMethodEClass, KAFKA_INTEGRATION_METHOD__RESPONSE_SCHEMA_NAME);
		createEAttribute(kafkaIntegrationMethodEClass, KAFKA_INTEGRATION_METHOD__USE_DEDICATED_PARTITION);
		createEAttribute(kafkaIntegrationMethodEClass, KAFKA_INTEGRATION_METHOD__SEND_RESPONSE);

		jmsIntegrationMethodEClass = createEClass(JMS_INTEGRATION_METHOD);
		createEAttribute(jmsIntegrationMethodEClass, JMS_INTEGRATION_METHOD__OPERATION_ID);
		createEAttribute(jmsIntegrationMethodEClass, JMS_INTEGRATION_METHOD__SEND_RESPONSE);

		jmsIntegrationBeanEClass = createEClass(JMS_INTEGRATION_BEAN);
		createEReference(jmsIntegrationBeanEClass, JMS_INTEGRATION_BEAN__REQUEST_DESTINATION);
		createEReference(jmsIntegrationBeanEClass, JMS_INTEGRATION_BEAN__RESPONSE_DESTINATION);

		jmsResourceEClass = createEClass(JMS_RESOURCE);
		createEAttribute(jmsResourceEClass, JMS_RESOURCE__NAME);
		createEAttribute(jmsResourceEClass, JMS_RESOURCE__TOPIC);

		// Create enums
		mediaTypeEnumerationEEnum = createEEnum(MEDIA_TYPE_ENUMERATION);
		httpMethodEnumerationEEnum = createEEnum(HTTP_METHOD_ENUMERATION);
	}

	/**
	 * @generated
	 */
	private boolean isInitialized;

	/**
	 * Complete the initialization of the package and its meta-model. This method is guarded to have no affect on any invocation but
	 * its first.
	 * @generated
	 */
	public void initializePackageContents() {
		if (isInitialized)
			return;

		isInitialized = true;

		// Initialize package
		setName(eNAME);
		setNsPrefix(eNS_PREFIX);
		setNsURI(eNS_URI);

		// Obtain other dependent packages
		final var theServicePackage = (ServicePackage) EPackage.Registry.INSTANCE.getEPackage(ServicePackage.eNS_URI);
		final var theBoundaryPackage = (BoundaryPackage) EPackage.Registry.INSTANCE.getEPackage(BoundaryPackage.eNS_URI);

		// Add supertypes to classes
		soapIntegrationBeanEClass.getESuperTypes().add(this.getAbstractIntegrationBean());
		restIntegrationBeanEClass.getESuperTypes().add(this.getAbstractIntegrationBean());
		abstractIntegrationBeanEClass.getESuperTypes().add(theServicePackage.getServiceBean());
		soapIntegrationMethodEClass.getESuperTypes().add(this.getAbstractIntegrationMethod());
		restIntegrationMethodEClass.getESuperTypes().add(this.getAbstractIntegrationMethod());
		abstractIntegrationMethodEClass.getESuperTypes().add(theServicePackage.getServiceMethod());
		rmiIntegrationMethodEClass.getESuperTypes().add(this.getAbstractIntegrationMethod());
		rmiIntegrationBeanEClass.getESuperTypes().add(this.getAbstractIntegrationBean());
		kafkaIntegrationBeanEClass.getESuperTypes().add(this.getAbstractIntegrationBean());
		kafkaIntegrationMethodEClass.getESuperTypes().add(this.getAbstractIntegrationMethod());
		jmsIntegrationMethodEClass.getESuperTypes().add(this.getAbstractIntegrationMethod());
		jmsIntegrationBeanEClass.getESuperTypes().add(this.getAbstractIntegrationBean());

		// Initialize classes and features; add operations and parameters
		initEClass(soapIntegrationBeanEClass, SOAPIntegrationBean.class, "SOAPIntegrationBean", !IS_ABSTRACT, !IS_INTERFACE,
				IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getSOAPIntegrationBean_ServiceName(), ecorePackage.getEString(), "serviceName", null, 0, 1,
				SOAPIntegrationBean.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getSOAPIntegrationBean_PortName(), ecorePackage.getEString(), "portName", null, 0, 1,
				SOAPIntegrationBean.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getSOAPIntegrationBean_RpcStype(), ecorePackage.getEBoolean(), "rpcStype", null, 0, 1,
				SOAPIntegrationBean.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getSOAPIntegrationBean_BareParameterStyle(), ecorePackage.getEBoolean(), "bareParameterStyle", null, 0, 1,
				SOAPIntegrationBean.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getSOAPIntegrationBean_PortTypeName(), ecorePackage.getEString(), "portTypeName", null, 0, 1,
				SOAPIntegrationBean.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);

		initEClass(restIntegrationBeanEClass, RESTIntegrationBean.class, "RESTIntegrationBean", !IS_ABSTRACT, !IS_INTERFACE,
				IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getRESTIntegrationBean_Path(), ecorePackage.getEString(), "path", null, 0, 1, RESTIntegrationBean.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(abstractIntegrationBeanEClass, AbstractIntegrationBean.class, "AbstractIntegrationBean", !IS_ABSTRACT,
				!IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getAbstractIntegrationBean_Methods(), this.getAbstractIntegrationMethod(),
				this.getAbstractIntegrationMethod_IntegrationBean(), "methods", null, 0, -1, AbstractIntegrationBean.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getAbstractIntegrationBean_ClientClassName(), ecorePackage.getEString(), "clientClassName", null, 0, 1,
				AbstractIntegrationBean.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getAbstractIntegrationBean_ProducerClassName(), ecorePackage.getEString(), "producerClassName", null, 0, 1,
				AbstractIntegrationBean.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);

		initEClass(soapIntegrationMethodEClass, SOAPIntegrationMethod.class, "SOAPIntegrationMethod", !IS_ABSTRACT, !IS_INTERFACE,
				IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getSOAPIntegrationMethod_OperationName(), ecorePackage.getEString(), "operationName", null, 0, 1,
				SOAPIntegrationMethod.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getSOAPIntegrationMethod_AddParameterAnnotations(), ecorePackage.getEBoolean(), "addParameterAnnotations",
				null, 0, 1, SOAPIntegrationMethod.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEAttribute(getSOAPIntegrationMethod_ReturnValueName(), ecorePackage.getEString(), "returnValueName", null, 0, 1,
				SOAPIntegrationMethod.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getSOAPIntegrationMethod_ReturnValuePartName(), ecorePackage.getEString(), "returnValuePartName", null, 0, 1,
				SOAPIntegrationMethod.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);

		initEClass(restIntegrationMethodEClass, RESTIntegrationMethod.class, "RESTIntegrationMethod", !IS_ABSTRACT, !IS_INTERFACE,
				IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getRESTIntegrationMethod_Path(), ecorePackage.getEString(), "path", null, 0, 1, RESTIntegrationMethod.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getRESTIntegrationMethod_HttpMethod(), this.getHttpMethodEnumeration(), "httpMethod", null, 0, 1,
				RESTIntegrationMethod.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getRESTIntegrationMethod_InputType(), this.getMediaTypeEnumeration(), "inputType", null, 0, 1,
				RESTIntegrationMethod.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getRESTIntegrationMethod_OutputType(), this.getMediaTypeEnumeration(), "outputType", null, 0, 1,
				RESTIntegrationMethod.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);

		initEClass(abstractIntegrationMethodEClass, AbstractIntegrationMethod.class, "AbstractIntegrationMethod", !IS_ABSTRACT,
				!IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getAbstractIntegrationMethod_IntegrationBean(), this.getAbstractIntegrationBean(),
				this.getAbstractIntegrationBean_Methods(), "integrationBean", null, 0, 1, AbstractIntegrationMethod.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getAbstractIntegrationMethod_BoundaryMethod(), theBoundaryPackage.getBoundaryMethod(), null, "boundaryMethod",
				null, 0, 1, AbstractIntegrationMethod.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE,
				IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getAbstractIntegrationMethod_StartNewThread(), ecorePackage.getEBoolean(), "startNewThread", null, 0, 1,
				AbstractIntegrationMethod.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);

		initEClass(rmiIntegrationMethodEClass, RMIIntegrationMethod.class, "RMIIntegrationMethod", !IS_ABSTRACT, !IS_INTERFACE,
				IS_GENERATED_INSTANCE_CLASS);

		initEClass(rmiIntegrationBeanEClass, RMIIntegrationBean.class, "RMIIntegrationBean", !IS_ABSTRACT, !IS_INTERFACE,
				IS_GENERATED_INSTANCE_CLASS);

		initEClass(kafkaIntegrationBeanEClass, KafkaIntegrationBean.class, "KafkaIntegrationBean", !IS_ABSTRACT, !IS_INTERFACE,
				IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getKafkaIntegrationBean_RequestTopic(), ecorePackage.getEString(), "requestTopic", null, 0, 1,
				KafkaIntegrationBean.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getKafkaIntegrationBean_ResponseTopic(), ecorePackage.getEString(), "responseTopic", null, 0, 1,
				KafkaIntegrationBean.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getKafkaIntegrationBean_ConsumerGroup(), ecorePackage.getEString(), "consumerGroup", null, 0, 1,
				KafkaIntegrationBean.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);

		initEClass(kafkaIntegrationMethodEClass, KafkaIntegrationMethod.class, "KafkaIntegrationMethod", !IS_ABSTRACT, !IS_INTERFACE,
				IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getKafkaIntegrationMethod_RequestSchemaName(), ecorePackage.getEString(), "requestSchemaName", null, 0, 1,
				KafkaIntegrationMethod.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getKafkaIntegrationMethod_ResponseSchemaName(), ecorePackage.getEString(), "responseSchemaName", null, 0, 1,
				KafkaIntegrationMethod.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getKafkaIntegrationMethod_UseDedicatedPartition(), ecorePackage.getEBoolean(), "useDedicatedPartition", null,
				0, 1, KafkaIntegrationMethod.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEAttribute(getKafkaIntegrationMethod_SendResponse(), ecorePackage.getEBoolean(), "sendResponse", null, 0, 1,
				KafkaIntegrationMethod.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);

		initEClass(jmsIntegrationMethodEClass, JMSIntegrationMethod.class, "JMSIntegrationMethod", !IS_ABSTRACT, !IS_INTERFACE,
				IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getJMSIntegrationMethod_OperationID(), ecorePackage.getEString(), "operationID", null, 0, 1,
				JMSIntegrationMethod.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getJMSIntegrationMethod_SendResponse(), ecorePackage.getEBoolean(), "sendResponse", null, 0, 1,
				JMSIntegrationMethod.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);

		initEClass(jmsIntegrationBeanEClass, JMSIntegrationBean.class, "JMSIntegrationBean", !IS_ABSTRACT, !IS_INTERFACE,
				IS_GENERATED_INSTANCE_CLASS);
		initEReference(getJMSIntegrationBean_RequestDestination(), this.getJMSResource(), null, "requestDestination", null, 0, 1,
				JMSIntegrationBean.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE,
				IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getJMSIntegrationBean_ResponseDestination(), this.getJMSResource(), null, "responseDestination", null, 0, 1,
				JMSIntegrationBean.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE,
				IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(jmsResourceEClass, JMSResource.class, "JMSResource", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getJMSResource_Name(), ecorePackage.getEString(), "name", null, 0, 1, JMSResource.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getJMSResource_Topic(), ecorePackage.getEBoolean(), "topic", null, 0, 1, JMSResource.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		// Initialize enums and add enum literals
		initEEnum(mediaTypeEnumerationEEnum, MediaTypeEnumeration.class, "MediaTypeEnumeration");
		addEEnumLiteral(mediaTypeEnumerationEEnum, MediaTypeEnumeration.XML);
		addEEnumLiteral(mediaTypeEnumerationEEnum, MediaTypeEnumeration.JSON);
		addEEnumLiteral(mediaTypeEnumerationEEnum, MediaTypeEnumeration.TEXT);
		addEEnumLiteral(mediaTypeEnumerationEEnum, MediaTypeEnumeration.BINARY);
		addEEnumLiteral(mediaTypeEnumerationEEnum, MediaTypeEnumeration.NONE);

		initEEnum(httpMethodEnumerationEEnum, HttpMethodEnumeration.class, "HttpMethodEnumeration");
		addEEnumLiteral(httpMethodEnumerationEEnum, HttpMethodEnumeration.GET);
		addEEnumLiteral(httpMethodEnumerationEEnum, HttpMethodEnumeration.PUT);
		addEEnumLiteral(httpMethodEnumerationEEnum, HttpMethodEnumeration.POST);
		addEEnumLiteral(httpMethodEnumerationEEnum, HttpMethodEnumeration.DELETE);

		// Create resource
		createResource(eNS_URI);
	}

}
