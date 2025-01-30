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
package net.codecadenza.eclipse.model.exchange.impl;

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
import net.codecadenza.eclipse.model.exchange.AssociationController;
import net.codecadenza.eclipse.model.exchange.ContentTypeEnumeration;
import net.codecadenza.eclipse.model.exchange.DataExchangeAttribute;
import net.codecadenza.eclipse.model.exchange.DataExchangeElement;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethodTypeEnumeration;
import net.codecadenza.eclipse.model.exchange.DataExchangeMode;
import net.codecadenza.eclipse.model.exchange.DataExchangeServiceBean;
import net.codecadenza.eclipse.model.exchange.DirectExchangeMode;
import net.codecadenza.eclipse.model.exchange.ExchangeFactory;
import net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute;
import net.codecadenza.eclipse.model.exchange.ExchangeMappingObject;
import net.codecadenza.eclipse.model.exchange.ExchangePackage;
import net.codecadenza.eclipse.model.exchange.FileExchangeMode;
import net.codecadenza.eclipse.model.exchange.FilterMethodParameter;
import net.codecadenza.eclipse.model.exchange.ParserImplementationEnumeration;
import net.codecadenza.eclipse.model.exchange.StringExchangeMode;
import net.codecadenza.eclipse.model.exchange.ValueListEntry;
import net.codecadenza.eclipse.model.integration.IntegrationPackage;
import net.codecadenza.eclipse.model.integration.impl.IntegrationPackageImpl;
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
public class ExchangePackageImpl extends EPackageImpl implements ExchangePackage {
	/**
	 * @generated
	 */
	private EClass dataExchangeServiceBeanEClass;

	/**
	 * @generated
	 */
	private EClass dataExchangeMethodEClass;

	/**
	 * @generated
	 */
	private EClass dataExchangeModeEClass;

	/**
	 * @generated
	 */
	private EClass stringExchangeModeEClass;

	/**
	 * @generated
	 */
	private EClass fileExchangeModeEClass;

	/**
	 * @generated
	 */
	private EClass dataExchangeElementEClass;

	/**
	 * @generated
	 */
	private EClass dataExchangeAttributeEClass;

	/**
	 * @generated
	 */
	private EClass valueListEntryEClass;

	/**
	 * @generated
	 */
	private EClass exchangeMappingObjectEClass;

	/**
	 * @generated
	 */
	private EClass exchangeMappingAttributeEClass;

	/**
	 * @generated
	 */
	private EClass filterMethodParameterEClass;

	/**
	 * @generated
	 */
	private EClass associationControllerEClass;

	/**
	 * @generated
	 */
	private EClass directExchangeModeEClass;

	/**
	 * @generated
	 */
	private EEnum dataExchangeMethodTypeEnumerationEEnum;

	/**
	 * @generated
	 */
	private EEnum contentTypeEnumerationEEnum;

	/**
	 * @generated
	 */
	private EEnum parserImplementationEnumerationEEnum;

	/**
	 * Create an instance of the model <b>Package</b>, registered with {@link org.eclipse.emf.ecore.EPackage.Registry
	 * EPackage.Registry} by the package URI value.
	 * <p>
	 * Note: The correct way to create the package is via the static factory method {@link #init init()}, which also performs
	 * initialization of the package, or returns the registered package, if one already exists.
	 * </p>
	 * @see org.eclipse.emf.ecore.EPackage.Registry
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#eNS_URI
	 * @see #init()
	 * @generated
	 */
	private ExchangePackageImpl() {
		super(eNS_URI, ExchangeFactory.eINSTANCE);
	}

	/**
	 * @generated
	 */
	private static boolean isInited;

	/**
	 * Create, register and initialize the <b>Package</b> for this model, and for any others upon which it depends. This method is
	 * used to initialize {@link ExchangePackage#eINSTANCE} when that field is accessed. Clients should not invoke it directly.
	 * Instead, they should simply access that field to obtain the package.
	 * @see #eNS_URI
	 * @see #createPackageContents()
	 * @see #initializePackageContents()
	 * @return the initialized exchange package
	 * @generated
	 */
	public static ExchangePackage init() {
		if (isInited)
			return (ExchangePackage) EPackage.Registry.INSTANCE.getEPackage(ExchangePackage.eNS_URI);

		// Obtain or create and register package
		final var theExchangePackage = (ExchangePackageImpl) (EPackage.Registry.INSTANCE.get(eNS_URI) instanceof ExchangePackageImpl
				? EPackage.Registry.INSTANCE.get(eNS_URI) : new ExchangePackageImpl());

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
		final var theIntegrationPackage = (IntegrationPackageImpl) (EPackage.Registry.INSTANCE
				.getEPackage(IntegrationPackage.eNS_URI) instanceof IntegrationPackageImpl
						? EPackage.Registry.INSTANCE.getEPackage(IntegrationPackage.eNS_URI) : IntegrationPackage.eINSTANCE);
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

		// Create package meta-data objects
		theExchangePackage.createPackageContents();
		theBoundaryPackage.createPackageContents();
		theClientPackage.createPackageContents();
		theRepositoryPackage.createPackageContents();
		theDbPackage.createPackageContents();
		theDomainPackage.createPackageContents();
		theDtoPackage.createPackageContents();
		theIntegrationPackage.createPackageContents();
		theJavaPackage.createPackageContents();
		theMappingPackage.createPackageContents();
		theProjectPackage.createPackageContents();
		theServicePackage.createPackageContents();

		// Initialize created meta-data
		theExchangePackage.initializePackageContents();
		theBoundaryPackage.initializePackageContents();
		theClientPackage.initializePackageContents();
		theRepositoryPackage.initializePackageContents();
		theDbPackage.initializePackageContents();
		theDomainPackage.initializePackageContents();
		theDtoPackage.initializePackageContents();
		theIntegrationPackage.initializePackageContents();
		theJavaPackage.initializePackageContents();
		theMappingPackage.initializePackageContents();
		theProjectPackage.initializePackageContents();
		theServicePackage.initializePackageContents();

		// Mark meta-data to indicate it can't be changed
		theExchangePackage.freeze();

		// Update the registry and return the package
		EPackage.Registry.INSTANCE.put(ExchangePackage.eNS_URI, theExchangePackage);
		return theExchangePackage;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeServiceBean()
	 * @generated
	 */
	@Override
	public EClass getDataExchangeServiceBean() {
		return dataExchangeServiceBeanEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeServiceBean_DataExchangeMethods()
	 * @generated
	 */
	@Override
	public EReference getDataExchangeServiceBean_DataExchangeMethods() {
		return (EReference) dataExchangeServiceBeanEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeMethod()
	 * @generated
	 */
	@Override
	public EClass getDataExchangeMethod() {
		return dataExchangeMethodEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeMethod_DataExchangeServiceBean()
	 * @generated
	 */
	@Override
	public EReference getDataExchangeMethod_DataExchangeServiceBean() {
		return (EReference) dataExchangeMethodEClass.getEStructuralFeatures().get(8);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeMethod_ProcessSingleObject()
	 * @generated
	 */
	@Override
	public EAttribute getDataExchangeMethod_ProcessSingleObject() {
		return (EAttribute) dataExchangeMethodEClass.getEStructuralFeatures().get(9);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeMethod_JoinedImportMethod()
	 * @generated
	 */
	@Override
	public EReference getDataExchangeMethod_JoinedImportMethod() {
		return (EReference) dataExchangeMethodEClass.getEStructuralFeatures().get(10);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeMethod_QuoteCharacter()
	 * @generated
	 */
	@Override
	public EAttribute getDataExchangeMethod_QuoteCharacter() {
		return (EAttribute) dataExchangeMethodEClass.getEStructuralFeatures().get(11);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeMethod_RecordSeparator()
	 * @generated
	 */
	@Override
	public EAttribute getDataExchangeMethod_RecordSeparator() {
		return (EAttribute) dataExchangeMethodEClass.getEStructuralFeatures().get(12);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeMethod_CommentCharacter()
	 * @generated
	 */
	@Override
	public EAttribute getDataExchangeMethod_CommentCharacter() {
		return (EAttribute) dataExchangeMethodEClass.getEStructuralFeatures().get(13);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeMethod_Delimiter()
	 * @generated
	 */
	@Override
	public EAttribute getDataExchangeMethod_Delimiter() {
		return (EAttribute) dataExchangeMethodEClass.getEStructuralFeatures().get(14);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeMethod_DefaultDateFormat()
	 * @generated
	 */
	@Override
	public EAttribute getDataExchangeMethod_DefaultDateFormat() {
		return (EAttribute) dataExchangeMethodEClass.getEStructuralFeatures().get(15);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeMethod_DefaultDateTimeFormat()
	 * @generated
	 */
	@Override
	public EAttribute getDataExchangeMethod_DefaultDateTimeFormat() {
		return (EAttribute) dataExchangeMethodEClass.getEStructuralFeatures().get(16);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeMethod_DefaultNumberFormat()
	 * @generated
	 */
	@Override
	public EAttribute getDataExchangeMethod_DefaultNumberFormat() {
		return (EAttribute) dataExchangeMethodEClass.getEStructuralFeatures().get(17);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeMethod_AssociationControllers()
	 * @generated
	 */
	@Override
	public EReference getDataExchangeMethod_AssociationControllers() {
		return (EReference) dataExchangeMethodEClass.getEStructuralFeatures().get(18);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeMethod_SchemaFileName()
	 * @generated
	 */
	@Override
	public EAttribute getDataExchangeMethod_SchemaFileName() {
		return (EAttribute) dataExchangeMethodEClass.getEStructuralFeatures().get(19);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeMethod_ExchangeMode()
	 * @generated
	 */
	@Override
	public EReference getDataExchangeMethod_ExchangeMode() {
		return (EReference) dataExchangeMethodEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeMethod_RootElement()
	 * @generated
	 */
	@Override
	public EReference getDataExchangeMethod_RootElement() {
		return (EReference) dataExchangeMethodEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeMethod_ContentType()
	 * @generated
	 */
	@Override
	public EAttribute getDataExchangeMethod_ContentType() {
		return (EAttribute) dataExchangeMethodEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeMethod_MethodType()
	 * @generated
	 */
	@Override
	public EAttribute getDataExchangeMethod_MethodType() {
		return (EAttribute) dataExchangeMethodEClass.getEStructuralFeatures().get(3);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeMethod_Parser()
	 * @generated
	 */
	@Override
	public EAttribute getDataExchangeMethod_Parser() {
		return (EAttribute) dataExchangeMethodEClass.getEStructuralFeatures().get(4);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeMethod_PerformValidation()
	 * @generated
	 */
	@Override
	public EAttribute getDataExchangeMethod_PerformValidation() {
		return (EAttribute) dataExchangeMethodEClass.getEStructuralFeatures().get(5);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeMethod_Charset()
	 * @generated
	 */
	@Override
	public EAttribute getDataExchangeMethod_Charset() {
		return (EAttribute) dataExchangeMethodEClass.getEStructuralFeatures().get(6);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeMethod_FormatOutput()
	 * @generated
	 */
	@Override
	public EAttribute getDataExchangeMethod_FormatOutput() {
		return (EAttribute) dataExchangeMethodEClass.getEStructuralFeatures().get(7);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeMode()
	 * @generated
	 */
	@Override
	public EClass getDataExchangeMode() {
		return dataExchangeModeEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeMode_MaxObjectsToBeProcessed()
	 * @generated
	 */
	@Override
	public EAttribute getDataExchangeMode_MaxObjectsToBeProcessed() {
		return (EAttribute) dataExchangeModeEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getStringExchangeMode()
	 * @generated
	 */
	@Override
	public EClass getStringExchangeMode() {
		return stringExchangeModeEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getFileExchangeMode()
	 * @generated
	 */
	@Override
	public EClass getFileExchangeMode() {
		return fileExchangeModeEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getFileExchangeMode_Path()
	 * @generated
	 */
	@Override
	public EAttribute getFileExchangeMode_Path() {
		return (EAttribute) fileExchangeModeEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getFileExchangeMode_FileNamePattern()
	 * @generated
	 */
	@Override
	public EAttribute getFileExchangeMode_FileNamePattern() {
		return (EAttribute) fileExchangeModeEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getFileExchangeMode_BlockSize()
	 * @generated
	 */
	@Override
	public EAttribute getFileExchangeMode_BlockSize() {
		return (EAttribute) fileExchangeModeEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getFileExchangeMode_NewTransactionPerFile()
	 * @generated
	 */
	@Override
	public EAttribute getFileExchangeMode_NewTransactionPerFile() {
		return (EAttribute) fileExchangeModeEClass.getEStructuralFeatures().get(3);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getFileExchangeMode_DeleteAfterImport()
	 * @generated
	 */
	@Override
	public EAttribute getFileExchangeMode_DeleteAfterImport() {
		return (EAttribute) fileExchangeModeEClass.getEStructuralFeatures().get(4);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getFileExchangeMode_TargetPathAfterImport()
	 * @generated
	 */
	@Override
	public EAttribute getFileExchangeMode_TargetPathAfterImport() {
		return (EAttribute) fileExchangeModeEClass.getEStructuralFeatures().get(5);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeElement()
	 * @generated
	 */
	@Override
	public EClass getDataExchangeElement() {
		return dataExchangeElementEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeElement_Name()
	 * @generated
	 */
	@Override
	public EAttribute getDataExchangeElement_Name() {
		return (EAttribute) dataExchangeElementEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeElement_MinOccurrences()
	 * @generated
	 */
	@Override
	public EAttribute getDataExchangeElement_MinOccurrences() {
		return (EAttribute) dataExchangeElementEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeElement_MaxOccurrences()
	 * @generated
	 */
	@Override
	public EAttribute getDataExchangeElement_MaxOccurrences() {
		return (EAttribute) dataExchangeElementEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeElement_ElementOrder()
	 * @generated
	 */
	@Override
	public EAttribute getDataExchangeElement_ElementOrder() {
		return (EAttribute) dataExchangeElementEClass.getEStructuralFeatures().get(3);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeElement_TypeName()
	 * @generated
	 */
	@Override
	public EAttribute getDataExchangeElement_TypeName() {
		return (EAttribute) dataExchangeElementEClass.getEStructuralFeatures().get(4);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeElement_WrapperElementName()
	 * @generated
	 */
	@Override
	public EAttribute getDataExchangeElement_WrapperElementName() {
		return (EAttribute) dataExchangeElementEClass.getEStructuralFeatures().get(5);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeElement_DataExchangeMethod()
	 * @generated
	 */
	@Override
	public EReference getDataExchangeElement_DataExchangeMethod() {
		return (EReference) dataExchangeElementEClass.getEStructuralFeatures().get(6);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeElement_ValueListEntries()
	 * @generated
	 */
	@Override
	public EReference getDataExchangeElement_ValueListEntries() {
		return (EReference) dataExchangeElementEClass.getEStructuralFeatures().get(7);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeElement_Attributes()
	 * @generated
	 */
	@Override
	public EReference getDataExchangeElement_Attributes() {
		return (EReference) dataExchangeElementEClass.getEStructuralFeatures().get(8);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeElement_MappingAttribute()
	 * @generated
	 */
	@Override
	public EReference getDataExchangeElement_MappingAttribute() {
		return (EReference) dataExchangeElementEClass.getEStructuralFeatures().get(9);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeElement_SubElements()
	 * @generated
	 */
	@Override
	public EReference getDataExchangeElement_SubElements() {
		return (EReference) dataExchangeElementEClass.getEStructuralFeatures().get(10);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeElement_ParentElement()
	 * @generated
	 */
	@Override
	public EReference getDataExchangeElement_ParentElement() {
		return (EReference) dataExchangeElementEClass.getEStructuralFeatures().get(11);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeElement_DataType()
	 * @generated
	 */
	@Override
	public EReference getDataExchangeElement_DataType() {
		return (EReference) dataExchangeElementEClass.getEStructuralFeatures().get(12);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeElement_MappingObject()
	 * @generated
	 */
	@Override
	public EReference getDataExchangeElement_MappingObject() {
		return (EReference) dataExchangeElementEClass.getEStructuralFeatures().get(13);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeElement_Container()
	 * @generated
	 */
	@Override
	public EAttribute getDataExchangeElement_Container() {
		return (EAttribute) dataExchangeElementEClass.getEStructuralFeatures().get(14);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeElement_DisableExternalMapping()
	 * @generated
	 */
	@Override
	public EAttribute getDataExchangeElement_DisableExternalMapping() {
		return (EAttribute) dataExchangeElementEClass.getEStructuralFeatures().get(15);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeElement_UsedForCustomQuery()
	 * @generated
	 */
	@Override
	public EAttribute getDataExchangeElement_UsedForCustomQuery() {
		return (EAttribute) dataExchangeElementEClass.getEStructuralFeatures().get(16);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeAttribute()
	 * @generated
	 */
	@Override
	public EClass getDataExchangeAttribute() {
		return dataExchangeAttributeEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeAttribute_ValueListEntries()
	 * @generated
	 */
	@Override
	public EReference getDataExchangeAttribute_ValueListEntries() {
		return (EReference) dataExchangeAttributeEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeAttribute_Name()
	 * @generated
	 */
	@Override
	public EAttribute getDataExchangeAttribute_Name() {
		return (EAttribute) dataExchangeAttributeEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeAttribute_Visible()
	 * @generated
	 */
	@Override
	public EAttribute getDataExchangeAttribute_Visible() {
		return (EAttribute) dataExchangeAttributeEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeAttribute_Optional()
	 * @generated
	 */
	@Override
	public EAttribute getDataExchangeAttribute_Optional() {
		return (EAttribute) dataExchangeAttributeEClass.getEStructuralFeatures().get(3);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeAttribute_AttributeOrder()
	 * @generated
	 */
	@Override
	public EAttribute getDataExchangeAttribute_AttributeOrder() {
		return (EAttribute) dataExchangeAttributeEClass.getEStructuralFeatures().get(4);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeAttribute_Readonly()
	 * @generated
	 */
	@Override
	public EAttribute getDataExchangeAttribute_Readonly() {
		return (EAttribute) dataExchangeAttributeEClass.getEStructuralFeatures().get(5);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeAttribute_Element()
	 * @generated
	 */
	@Override
	public EReference getDataExchangeAttribute_Element() {
		return (EReference) dataExchangeAttributeEClass.getEStructuralFeatures().get(6);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeAttribute_MappingAttribute()
	 * @generated
	 */
	@Override
	public EReference getDataExchangeAttribute_MappingAttribute() {
		return (EReference) dataExchangeAttributeEClass.getEStructuralFeatures().get(7);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeAttribute_DataType()
	 * @generated
	 */
	@Override
	public EReference getDataExchangeAttribute_DataType() {
		return (EReference) dataExchangeAttributeEClass.getEStructuralFeatures().get(8);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeAttribute_Format()
	 * @generated
	 */
	@Override
	public EAttribute getDataExchangeAttribute_Format() {
		return (EAttribute) dataExchangeAttributeEClass.getEStructuralFeatures().get(9);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeAttribute_DisableExternalMapping()
	 * @generated
	 */
	@Override
	public EAttribute getDataExchangeAttribute_DisableExternalMapping() {
		return (EAttribute) dataExchangeAttributeEClass.getEStructuralFeatures().get(10);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeAttribute_UsedForCustomQuery()
	 * @generated
	 */
	@Override
	public EAttribute getDataExchangeAttribute_UsedForCustomQuery() {
		return (EAttribute) dataExchangeAttributeEClass.getEStructuralFeatures().get(11);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getValueListEntry()
	 * @generated
	 */
	@Override
	public EClass getValueListEntry() {
		return valueListEntryEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getValueListEntry_ItemText()
	 * @generated
	 */
	@Override
	public EAttribute getValueListEntry_ItemText() {
		return (EAttribute) valueListEntryEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getExchangeMappingObject()
	 * @generated
	 */
	@Override
	public EClass getExchangeMappingObject() {
		return exchangeMappingObjectEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getExchangeMappingObject_Attributes()
	 * @generated
	 */
	@Override
	public EReference getExchangeMappingObject_Attributes() {
		return (EReference) exchangeMappingObjectEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getExchangeMappingObject_DeleteAllItems()
	 * @generated
	 */
	@Override
	public EAttribute getExchangeMappingObject_DeleteAllItems() {
		return (EAttribute) exchangeMappingObjectEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getExchangeMappingObject_AddNewItems()
	 * @generated
	 */
	@Override
	public EAttribute getExchangeMappingObject_AddNewItems() {
		return (EAttribute) exchangeMappingObjectEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getExchangeMappingObject_UpdateExistingItems()
	 * @generated
	 */
	@Override
	public EAttribute getExchangeMappingObject_UpdateExistingItems() {
		return (EAttribute) exchangeMappingObjectEClass.getEStructuralFeatures().get(3);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getExchangeMappingAttribute()
	 * @generated
	 */
	@Override
	public EClass getExchangeMappingAttribute() {
		return exchangeMappingAttributeEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getExchangeMappingAttribute_ExchangeMappingObject()
	 * @generated
	 */
	@Override
	public EReference getExchangeMappingAttribute_ExchangeMappingObject() {
		return (EReference) exchangeMappingAttributeEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getExchangeMappingAttribute_Insertable()
	 * @generated
	 */
	@Override
	public EAttribute getExchangeMappingAttribute_Insertable() {
		return (EAttribute) exchangeMappingAttributeEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getExchangeMappingAttribute_Updatable()
	 * @generated
	 */
	@Override
	public EAttribute getExchangeMappingAttribute_Updatable() {
		return (EAttribute) exchangeMappingAttributeEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getExchangeMappingAttribute_SelectionListStatement()
	 * @generated
	 */
	@Override
	public EAttribute getExchangeMappingAttribute_SelectionListStatement() {
		return (EAttribute) exchangeMappingAttributeEClass.getEStructuralFeatures().get(3);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getExchangeMappingAttribute_DeleteAllItems()
	 * @generated
	 */
	@Override
	public EAttribute getExchangeMappingAttribute_DeleteAllItems() {
		return (EAttribute) exchangeMappingAttributeEClass.getEStructuralFeatures().get(4);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getExchangeMappingAttribute_AddNewItems()
	 * @generated
	 */
	@Override
	public EAttribute getExchangeMappingAttribute_AddNewItems() {
		return (EAttribute) exchangeMappingAttributeEClass.getEStructuralFeatures().get(5);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getExchangeMappingAttribute_JoinAttribute()
	 * @generated
	 */
	@Override
	public EAttribute getExchangeMappingAttribute_JoinAttribute() {
		return (EAttribute) exchangeMappingAttributeEClass.getEStructuralFeatures().get(6);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getExchangeMappingAttribute_UpdateExistingItems()
	 * @generated
	 */
	@Override
	public EAttribute getExchangeMappingAttribute_UpdateExistingItems() {
		return (EAttribute) exchangeMappingAttributeEClass.getEStructuralFeatures().get(7);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getFilterMethodParameter()
	 * @generated
	 */
	@Override
	public EClass getFilterMethodParameter() {
		return filterMethodParameterEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getFilterMethodParameter_DomainAttribute()
	 * @generated
	 */
	@Override
	public EReference getFilterMethodParameter_DomainAttribute() {
		return (EReference) filterMethodParameterEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getFilterMethodParameter_Association()
	 * @generated
	 */
	@Override
	public EReference getFilterMethodParameter_Association() {
		return (EReference) filterMethodParameterEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getFilterMethodParameter_AssociationList()
	 * @generated
	 */
	@Override
	public EReference getFilterMethodParameter_AssociationList() {
		return (EReference) filterMethodParameterEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getFilterMethodParameter_Operator()
	 * @generated
	 */
	@Override
	public EAttribute getFilterMethodParameter_Operator() {
		return (EAttribute) filterMethodParameterEClass.getEStructuralFeatures().get(3);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getAssociationController()
	 * @generated
	 */
	@Override
	public EClass getAssociationController() {
		return associationControllerEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getAssociationController_QueryAttributes()
	 * @generated
	 */
	@Override
	public EReference getAssociationController_QueryAttributes() {
		return (EReference) associationControllerEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getAssociationController_Association()
	 * @generated
	 */
	@Override
	public EReference getAssociationController_Association() {
		return (EReference) associationControllerEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getAssociationController_PersistAttributes()
	 * @generated
	 */
	@Override
	public EReference getAssociationController_PersistAttributes() {
		return (EReference) associationControllerEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDirectExchangeMode()
	 * @generated
	 */
	@Override
	public EClass getDirectExchangeMode() {
		return directExchangeModeEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeMethodTypeEnumeration()
	 * @generated
	 */
	@Override
	public EEnum getDataExchangeMethodTypeEnumeration() {
		return dataExchangeMethodTypeEnumerationEEnum;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getContentTypeEnumeration()
	 * @generated
	 */
	@Override
	public EEnum getContentTypeEnumeration() {
		return contentTypeEnumerationEEnum;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getParserImplementationEnumeration()
	 * @generated
	 */
	@Override
	public EEnum getParserImplementationEnumeration() {
		return parserImplementationEnumerationEEnum;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getExchangeFactory()
	 * @generated
	 */
	@Override
	public ExchangeFactory getExchangeFactory() {
		return (ExchangeFactory) getEFactoryInstance();
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
		dataExchangeServiceBeanEClass = createEClass(DATA_EXCHANGE_SERVICE_BEAN);
		createEReference(dataExchangeServiceBeanEClass, DATA_EXCHANGE_SERVICE_BEAN__DATA_EXCHANGE_METHODS);

		dataExchangeMethodEClass = createEClass(DATA_EXCHANGE_METHOD);
		createEReference(dataExchangeMethodEClass, DATA_EXCHANGE_METHOD__EXCHANGE_MODE);
		createEReference(dataExchangeMethodEClass, DATA_EXCHANGE_METHOD__ROOT_ELEMENT);
		createEAttribute(dataExchangeMethodEClass, DATA_EXCHANGE_METHOD__CONTENT_TYPE);
		createEAttribute(dataExchangeMethodEClass, DATA_EXCHANGE_METHOD__METHOD_TYPE);
		createEAttribute(dataExchangeMethodEClass, DATA_EXCHANGE_METHOD__PARSER);
		createEAttribute(dataExchangeMethodEClass, DATA_EXCHANGE_METHOD__PERFORM_VALIDATION);
		createEAttribute(dataExchangeMethodEClass, DATA_EXCHANGE_METHOD__CHARSET);
		createEAttribute(dataExchangeMethodEClass, DATA_EXCHANGE_METHOD__FORMAT_OUTPUT);
		createEReference(dataExchangeMethodEClass, DATA_EXCHANGE_METHOD__DATA_EXCHANGE_SERVICE_BEAN);
		createEAttribute(dataExchangeMethodEClass, DATA_EXCHANGE_METHOD__PROCESS_SINGLE_OBJECT);
		createEReference(dataExchangeMethodEClass, DATA_EXCHANGE_METHOD__JOINED_IMPORT_METHOD);
		createEAttribute(dataExchangeMethodEClass, DATA_EXCHANGE_METHOD__QUOTE_CHARACTER);
		createEAttribute(dataExchangeMethodEClass, DATA_EXCHANGE_METHOD__RECORD_SEPARATOR);
		createEAttribute(dataExchangeMethodEClass, DATA_EXCHANGE_METHOD__COMMENT_CHARACTER);
		createEAttribute(dataExchangeMethodEClass, DATA_EXCHANGE_METHOD__DELIMITER);
		createEAttribute(dataExchangeMethodEClass, DATA_EXCHANGE_METHOD__DEFAULT_DATE_FORMAT);
		createEAttribute(dataExchangeMethodEClass, DATA_EXCHANGE_METHOD__DEFAULT_DATE_TIME_FORMAT);
		createEAttribute(dataExchangeMethodEClass, DATA_EXCHANGE_METHOD__DEFAULT_NUMBER_FORMAT);
		createEReference(dataExchangeMethodEClass, DATA_EXCHANGE_METHOD__ASSOCIATION_CONTROLLERS);
		createEAttribute(dataExchangeMethodEClass, DATA_EXCHANGE_METHOD__SCHEMA_FILE_NAME);

		dataExchangeModeEClass = createEClass(DATA_EXCHANGE_MODE);
		createEAttribute(dataExchangeModeEClass, DATA_EXCHANGE_MODE__MAX_OBJECTS_TO_BE_PROCESSED);

		stringExchangeModeEClass = createEClass(STRING_EXCHANGE_MODE);

		fileExchangeModeEClass = createEClass(FILE_EXCHANGE_MODE);
		createEAttribute(fileExchangeModeEClass, FILE_EXCHANGE_MODE__PATH);
		createEAttribute(fileExchangeModeEClass, FILE_EXCHANGE_MODE__FILE_NAME_PATTERN);
		createEAttribute(fileExchangeModeEClass, FILE_EXCHANGE_MODE__BLOCK_SIZE);
		createEAttribute(fileExchangeModeEClass, FILE_EXCHANGE_MODE__NEW_TRANSACTION_PER_FILE);
		createEAttribute(fileExchangeModeEClass, FILE_EXCHANGE_MODE__DELETE_AFTER_IMPORT);
		createEAttribute(fileExchangeModeEClass, FILE_EXCHANGE_MODE__TARGET_PATH_AFTER_IMPORT);

		dataExchangeElementEClass = createEClass(DATA_EXCHANGE_ELEMENT);
		createEAttribute(dataExchangeElementEClass, DATA_EXCHANGE_ELEMENT__NAME);
		createEAttribute(dataExchangeElementEClass, DATA_EXCHANGE_ELEMENT__MIN_OCCURRENCES);
		createEAttribute(dataExchangeElementEClass, DATA_EXCHANGE_ELEMENT__MAX_OCCURRENCES);
		createEAttribute(dataExchangeElementEClass, DATA_EXCHANGE_ELEMENT__ELEMENT_ORDER);
		createEAttribute(dataExchangeElementEClass, DATA_EXCHANGE_ELEMENT__TYPE_NAME);
		createEAttribute(dataExchangeElementEClass, DATA_EXCHANGE_ELEMENT__WRAPPER_ELEMENT_NAME);
		createEReference(dataExchangeElementEClass, DATA_EXCHANGE_ELEMENT__DATA_EXCHANGE_METHOD);
		createEReference(dataExchangeElementEClass, DATA_EXCHANGE_ELEMENT__VALUE_LIST_ENTRIES);
		createEReference(dataExchangeElementEClass, DATA_EXCHANGE_ELEMENT__ATTRIBUTES);
		createEReference(dataExchangeElementEClass, DATA_EXCHANGE_ELEMENT__MAPPING_ATTRIBUTE);
		createEReference(dataExchangeElementEClass, DATA_EXCHANGE_ELEMENT__SUB_ELEMENTS);
		createEReference(dataExchangeElementEClass, DATA_EXCHANGE_ELEMENT__PARENT_ELEMENT);
		createEReference(dataExchangeElementEClass, DATA_EXCHANGE_ELEMENT__DATA_TYPE);
		createEReference(dataExchangeElementEClass, DATA_EXCHANGE_ELEMENT__MAPPING_OBJECT);
		createEAttribute(dataExchangeElementEClass, DATA_EXCHANGE_ELEMENT__CONTAINER);
		createEAttribute(dataExchangeElementEClass, DATA_EXCHANGE_ELEMENT__DISABLE_EXTERNAL_MAPPING);
		createEAttribute(dataExchangeElementEClass, DATA_EXCHANGE_ELEMENT__USED_FOR_CUSTOM_QUERY);

		dataExchangeAttributeEClass = createEClass(DATA_EXCHANGE_ATTRIBUTE);
		createEReference(dataExchangeAttributeEClass, DATA_EXCHANGE_ATTRIBUTE__VALUE_LIST_ENTRIES);
		createEAttribute(dataExchangeAttributeEClass, DATA_EXCHANGE_ATTRIBUTE__NAME);
		createEAttribute(dataExchangeAttributeEClass, DATA_EXCHANGE_ATTRIBUTE__VISIBLE);
		createEAttribute(dataExchangeAttributeEClass, DATA_EXCHANGE_ATTRIBUTE__OPTIONAL);
		createEAttribute(dataExchangeAttributeEClass, DATA_EXCHANGE_ATTRIBUTE__ATTRIBUTE_ORDER);
		createEAttribute(dataExchangeAttributeEClass, DATA_EXCHANGE_ATTRIBUTE__READONLY);
		createEReference(dataExchangeAttributeEClass, DATA_EXCHANGE_ATTRIBUTE__ELEMENT);
		createEReference(dataExchangeAttributeEClass, DATA_EXCHANGE_ATTRIBUTE__MAPPING_ATTRIBUTE);
		createEReference(dataExchangeAttributeEClass, DATA_EXCHANGE_ATTRIBUTE__DATA_TYPE);
		createEAttribute(dataExchangeAttributeEClass, DATA_EXCHANGE_ATTRIBUTE__FORMAT);
		createEAttribute(dataExchangeAttributeEClass, DATA_EXCHANGE_ATTRIBUTE__DISABLE_EXTERNAL_MAPPING);
		createEAttribute(dataExchangeAttributeEClass, DATA_EXCHANGE_ATTRIBUTE__USED_FOR_CUSTOM_QUERY);

		valueListEntryEClass = createEClass(VALUE_LIST_ENTRY);
		createEAttribute(valueListEntryEClass, VALUE_LIST_ENTRY__ITEM_TEXT);

		exchangeMappingObjectEClass = createEClass(EXCHANGE_MAPPING_OBJECT);
		createEReference(exchangeMappingObjectEClass, EXCHANGE_MAPPING_OBJECT__ATTRIBUTES);
		createEAttribute(exchangeMappingObjectEClass, EXCHANGE_MAPPING_OBJECT__DELETE_ALL_ITEMS);
		createEAttribute(exchangeMappingObjectEClass, EXCHANGE_MAPPING_OBJECT__ADD_NEW_ITEMS);
		createEAttribute(exchangeMappingObjectEClass, EXCHANGE_MAPPING_OBJECT__UPDATE_EXISTING_ITEMS);

		exchangeMappingAttributeEClass = createEClass(EXCHANGE_MAPPING_ATTRIBUTE);
		createEReference(exchangeMappingAttributeEClass, EXCHANGE_MAPPING_ATTRIBUTE__EXCHANGE_MAPPING_OBJECT);
		createEAttribute(exchangeMappingAttributeEClass, EXCHANGE_MAPPING_ATTRIBUTE__INSERTABLE);
		createEAttribute(exchangeMappingAttributeEClass, EXCHANGE_MAPPING_ATTRIBUTE__UPDATABLE);
		createEAttribute(exchangeMappingAttributeEClass, EXCHANGE_MAPPING_ATTRIBUTE__SELECTION_LIST_STATEMENT);
		createEAttribute(exchangeMappingAttributeEClass, EXCHANGE_MAPPING_ATTRIBUTE__DELETE_ALL_ITEMS);
		createEAttribute(exchangeMappingAttributeEClass, EXCHANGE_MAPPING_ATTRIBUTE__ADD_NEW_ITEMS);
		createEAttribute(exchangeMappingAttributeEClass, EXCHANGE_MAPPING_ATTRIBUTE__JOIN_ATTRIBUTE);
		createEAttribute(exchangeMappingAttributeEClass, EXCHANGE_MAPPING_ATTRIBUTE__UPDATE_EXISTING_ITEMS);

		filterMethodParameterEClass = createEClass(FILTER_METHOD_PARAMETER);
		createEReference(filterMethodParameterEClass, FILTER_METHOD_PARAMETER__DOMAIN_ATTRIBUTE);
		createEReference(filterMethodParameterEClass, FILTER_METHOD_PARAMETER__ASSOCIATION);
		createEReference(filterMethodParameterEClass, FILTER_METHOD_PARAMETER__ASSOCIATION_LIST);
		createEAttribute(filterMethodParameterEClass, FILTER_METHOD_PARAMETER__OPERATOR);

		associationControllerEClass = createEClass(ASSOCIATION_CONTROLLER);
		createEReference(associationControllerEClass, ASSOCIATION_CONTROLLER__QUERY_ATTRIBUTES);
		createEReference(associationControllerEClass, ASSOCIATION_CONTROLLER__ASSOCIATION);
		createEReference(associationControllerEClass, ASSOCIATION_CONTROLLER__PERSIST_ATTRIBUTES);

		directExchangeModeEClass = createEClass(DIRECT_EXCHANGE_MODE);

		// Create enums
		dataExchangeMethodTypeEnumerationEEnum = createEEnum(DATA_EXCHANGE_METHOD_TYPE_ENUMERATION);
		contentTypeEnumerationEEnum = createEEnum(CONTENT_TYPE_ENUMERATION);
		parserImplementationEnumerationEEnum = createEEnum(PARSER_IMPLEMENTATION_ENUMERATION);
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
		final var theJavaPackage = (JavaPackage) EPackage.Registry.INSTANCE.getEPackage(JavaPackage.eNS_URI);
		final var theMappingPackage = (MappingPackage) EPackage.Registry.INSTANCE.getEPackage(MappingPackage.eNS_URI);
		final var theDomainPackage = (DomainPackage) EPackage.Registry.INSTANCE.getEPackage(DomainPackage.eNS_URI);

		// Add supertypes to classes
		dataExchangeServiceBeanEClass.getESuperTypes().add(theServicePackage.getServiceBean());
		dataExchangeMethodEClass.getESuperTypes().add(theServicePackage.getServiceMethod());
		stringExchangeModeEClass.getESuperTypes().add(this.getDataExchangeMode());
		fileExchangeModeEClass.getESuperTypes().add(this.getDataExchangeMode());
		exchangeMappingObjectEClass.getESuperTypes().add(theMappingPackage.getMappingObject());
		exchangeMappingAttributeEClass.getESuperTypes().add(theMappingPackage.getMappingAttribute());
		filterMethodParameterEClass.getESuperTypes().add(theJavaPackage.getMethodParameter());
		directExchangeModeEClass.getESuperTypes().add(this.getDataExchangeMode());

		// Initialize classes and features; add operations and parameters
		initEClass(dataExchangeServiceBeanEClass, DataExchangeServiceBean.class, "DataExchangeServiceBean", !IS_ABSTRACT,
				!IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getDataExchangeServiceBean_DataExchangeMethods(), this.getDataExchangeMethod(),
				this.getDataExchangeMethod_DataExchangeServiceBean(), "dataExchangeMethods", null, 0, -1, DataExchangeServiceBean.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);

		initEClass(dataExchangeMethodEClass, DataExchangeMethod.class, "DataExchangeMethod", !IS_ABSTRACT, !IS_INTERFACE,
				IS_GENERATED_INSTANCE_CLASS);
		initEReference(getDataExchangeMethod_ExchangeMode(), this.getDataExchangeMode(), null, "exchangeMode", null, 0, 1,
				DataExchangeMethod.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE,
				IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getDataExchangeMethod_RootElement(), this.getDataExchangeElement(),
				this.getDataExchangeElement_DataExchangeMethod(), "rootElement", null, 0, 1, DataExchangeMethod.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getDataExchangeMethod_ContentType(), this.getContentTypeEnumeration(), "contentType", null, 0, 1,
				DataExchangeMethod.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getDataExchangeMethod_MethodType(), this.getDataExchangeMethodTypeEnumeration(), "methodType", null, 0, 1,
				DataExchangeMethod.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getDataExchangeMethod_Parser(), this.getParserImplementationEnumeration(), "parser", null, 0, 1,
				DataExchangeMethod.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getDataExchangeMethod_PerformValidation(), ecorePackage.getEBoolean(), "performValidation", null, 0, 1,
				DataExchangeMethod.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getDataExchangeMethod_Charset(), ecorePackage.getEString(), "charset", null, 0, 1, DataExchangeMethod.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getDataExchangeMethod_FormatOutput(), ecorePackage.getEBoolean(), "formatOutput", null, 0, 1,
				DataExchangeMethod.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEReference(getDataExchangeMethod_DataExchangeServiceBean(), this.getDataExchangeServiceBean(),
				this.getDataExchangeServiceBean_DataExchangeMethods(), "dataExchangeServiceBean", null, 0, 1, DataExchangeMethod.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getDataExchangeMethod_ProcessSingleObject(), ecorePackage.getEBoolean(), "processSingleObject", null, 0, 1,
				DataExchangeMethod.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEReference(getDataExchangeMethod_JoinedImportMethod(), this.getDataExchangeMethod(), null, "joinedImportMethod", null, 0,
				1, DataExchangeMethod.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES,
				!IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getDataExchangeMethod_QuoteCharacter(), ecorePackage.getEChar(), "quoteCharacter", null, 0, 1,
				DataExchangeMethod.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getDataExchangeMethod_RecordSeparator(), ecorePackage.getEString(), "recordSeparator", null, 0, 1,
				DataExchangeMethod.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getDataExchangeMethod_CommentCharacter(), ecorePackage.getEChar(), "commentCharacter", null, 0, 1,
				DataExchangeMethod.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getDataExchangeMethod_Delimiter(), ecorePackage.getEChar(), "delimiter", null, 0, 1, DataExchangeMethod.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getDataExchangeMethod_DefaultDateFormat(), ecorePackage.getEString(), "defaultDateFormat", null, 0, 1,
				DataExchangeMethod.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getDataExchangeMethod_DefaultDateTimeFormat(), ecorePackage.getEString(), "defaultDateTimeFormat", null, 0, 1,
				DataExchangeMethod.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getDataExchangeMethod_DefaultNumberFormat(), ecorePackage.getEString(), "defaultNumberFormat", null, 0, 1,
				DataExchangeMethod.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEReference(getDataExchangeMethod_AssociationControllers(), this.getAssociationController(), null,
				"associationControllers", null, 0, -1, DataExchangeMethod.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE,
				!IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getDataExchangeMethod_SchemaFileName(), ecorePackage.getEString(), "schemaFileName", null, 0, 1,
				DataExchangeMethod.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);

		initEClass(dataExchangeModeEClass, DataExchangeMode.class, "DataExchangeMode", IS_ABSTRACT, !IS_INTERFACE,
				IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getDataExchangeMode_MaxObjectsToBeProcessed(), ecorePackage.getEIntegerObject(), "maxObjectsToBeProcessed",
				null, 0, 1, DataExchangeMode.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);

		initEClass(stringExchangeModeEClass, StringExchangeMode.class, "StringExchangeMode", !IS_ABSTRACT, !IS_INTERFACE,
				IS_GENERATED_INSTANCE_CLASS);

		initEClass(fileExchangeModeEClass, FileExchangeMode.class, "FileExchangeMode", !IS_ABSTRACT, !IS_INTERFACE,
				IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getFileExchangeMode_Path(), ecorePackage.getEString(), "path", null, 0, 1, FileExchangeMode.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getFileExchangeMode_FileNamePattern(), ecorePackage.getEString(), "fileNamePattern", null, 0, 1,
				FileExchangeMode.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getFileExchangeMode_BlockSize(), ecorePackage.getEIntegerObject(), "blockSize", null, 0, 1,
				FileExchangeMode.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getFileExchangeMode_NewTransactionPerFile(), ecorePackage.getEBoolean(), "newTransactionPerFile", null, 0, 1,
				FileExchangeMode.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getFileExchangeMode_DeleteAfterImport(), ecorePackage.getEBoolean(), "deleteAfterImport", null, 0, 1,
				FileExchangeMode.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getFileExchangeMode_TargetPathAfterImport(), ecorePackage.getEString(), "targetPathAfterImport", null, 0, 1,
				FileExchangeMode.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);

		initEClass(dataExchangeElementEClass, DataExchangeElement.class, "DataExchangeElement", !IS_ABSTRACT, !IS_INTERFACE,
				IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getDataExchangeElement_Name(), ecorePackage.getEString(), "name", null, 0, 1, DataExchangeElement.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getDataExchangeElement_MinOccurrences(), ecorePackage.getEIntegerObject(), "minOccurrences", null, 0, 1,
				DataExchangeElement.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getDataExchangeElement_MaxOccurrences(), ecorePackage.getEIntegerObject(), "maxOccurrences", null, 0, 1,
				DataExchangeElement.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getDataExchangeElement_ElementOrder(), ecorePackage.getEIntegerObject(), "elementOrder", null, 0, 1,
				DataExchangeElement.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getDataExchangeElement_TypeName(), ecorePackage.getEString(), "typeName", null, 0, 1,
				DataExchangeElement.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getDataExchangeElement_WrapperElementName(), ecorePackage.getEString(), "wrapperElementName", null, 0, 1,
				DataExchangeElement.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEReference(getDataExchangeElement_DataExchangeMethod(), this.getDataExchangeMethod(),
				this.getDataExchangeMethod_RootElement(), "dataExchangeMethod", null, 0, 1, DataExchangeElement.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getDataExchangeElement_ValueListEntries(), this.getValueListEntry(), null, "valueListEntries", null, 0, -1,
				DataExchangeElement.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE,
				IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getDataExchangeElement_Attributes(), this.getDataExchangeAttribute(), this.getDataExchangeAttribute_Element(),
				"attributes", null, 0, -1, DataExchangeElement.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE,
				!IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getDataExchangeElement_MappingAttribute(), this.getExchangeMappingAttribute(), null, "mappingAttribute", null,
				0, 1, DataExchangeElement.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES,
				!IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getDataExchangeElement_SubElements(), this.getDataExchangeElement(),
				this.getDataExchangeElement_ParentElement(), "subElements", null, 0, -1, DataExchangeElement.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getDataExchangeElement_ParentElement(), this.getDataExchangeElement(),
				this.getDataExchangeElement_SubElements(), "parentElement", null, 0, 1, DataExchangeElement.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getDataExchangeElement_DataType(), theJavaPackage.getJavaType(), null, "dataType", null, 0, 1,
				DataExchangeElement.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE,
				IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getDataExchangeElement_MappingObject(), this.getExchangeMappingObject(), null, "mappingObject", null, 0, 1,
				DataExchangeElement.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE,
				IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getDataExchangeElement_Container(), ecorePackage.getEBoolean(), "container", null, 0, 1,
				DataExchangeElement.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getDataExchangeElement_DisableExternalMapping(), ecorePackage.getEBoolean(), "disableExternalMapping", null, 0,
				1, DataExchangeElement.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getDataExchangeElement_UsedForCustomQuery(), ecorePackage.getEBoolean(), "usedForCustomQuery", null, 0, 1,
				DataExchangeElement.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);

		initEClass(dataExchangeAttributeEClass, DataExchangeAttribute.class, "DataExchangeAttribute", !IS_ABSTRACT, !IS_INTERFACE,
				IS_GENERATED_INSTANCE_CLASS);
		initEReference(getDataExchangeAttribute_ValueListEntries(), this.getValueListEntry(), null, "valueListEntries", null, 0, -1,
				DataExchangeAttribute.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES,
				!IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getDataExchangeAttribute_Name(), ecorePackage.getEString(), "name", null, 0, 1, DataExchangeAttribute.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getDataExchangeAttribute_Visible(), ecorePackage.getEBoolean(), "visible", null, 0, 1,
				DataExchangeAttribute.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getDataExchangeAttribute_Optional(), ecorePackage.getEBoolean(), "optional", null, 0, 1,
				DataExchangeAttribute.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getDataExchangeAttribute_AttributeOrder(), ecorePackage.getEIntegerObject(), "attributeOrder", null, 0, 1,
				DataExchangeAttribute.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getDataExchangeAttribute_Readonly(), ecorePackage.getEBoolean(), "readonly", null, 0, 1,
				DataExchangeAttribute.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEReference(getDataExchangeAttribute_Element(), this.getDataExchangeElement(), this.getDataExchangeElement_Attributes(),
				"element", null, 0, 1, DataExchangeAttribute.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE,
				!IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getDataExchangeAttribute_MappingAttribute(), this.getExchangeMappingAttribute(), null, "mappingAttribute",
				null, 0, 1, DataExchangeAttribute.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES,
				!IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getDataExchangeAttribute_DataType(), theJavaPackage.getJavaType(), null, "dataType", null, 0, 1,
				DataExchangeAttribute.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES,
				!IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getDataExchangeAttribute_Format(), ecorePackage.getEString(), "format", null, 0, 1,
				DataExchangeAttribute.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getDataExchangeAttribute_DisableExternalMapping(), ecorePackage.getEBoolean(), "disableExternalMapping", null,
				0, 1, DataExchangeAttribute.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEAttribute(getDataExchangeAttribute_UsedForCustomQuery(), ecorePackage.getEBoolean(), "usedForCustomQuery", null, 0, 1,
				DataExchangeAttribute.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);

		initEClass(valueListEntryEClass, ValueListEntry.class, "ValueListEntry", !IS_ABSTRACT, !IS_INTERFACE,
				IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getValueListEntry_ItemText(), ecorePackage.getEString(), "itemText", null, 0, 1, ValueListEntry.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(exchangeMappingObjectEClass, ExchangeMappingObject.class, "ExchangeMappingObject", !IS_ABSTRACT, !IS_INTERFACE,
				IS_GENERATED_INSTANCE_CLASS);
		initEReference(getExchangeMappingObject_Attributes(), this.getExchangeMappingAttribute(),
				this.getExchangeMappingAttribute_ExchangeMappingObject(), "attributes", null, 0, -1, ExchangeMappingObject.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getExchangeMappingObject_DeleteAllItems(), ecorePackage.getEBoolean(), "deleteAllItems", null, 0, 1,
				ExchangeMappingObject.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getExchangeMappingObject_AddNewItems(), ecorePackage.getEBoolean(), "addNewItems", null, 0, 1,
				ExchangeMappingObject.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getExchangeMappingObject_UpdateExistingItems(), ecorePackage.getEBoolean(), "updateExistingItems", null, 0, 1,
				ExchangeMappingObject.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);

		initEClass(exchangeMappingAttributeEClass, ExchangeMappingAttribute.class, "ExchangeMappingAttribute", !IS_ABSTRACT,
				!IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getExchangeMappingAttribute_ExchangeMappingObject(), this.getExchangeMappingObject(),
				this.getExchangeMappingObject_Attributes(), "exchangeMappingObject", null, 0, 1, ExchangeMappingAttribute.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getExchangeMappingAttribute_Insertable(), ecorePackage.getEBoolean(), "insertable", null, 0, 1,
				ExchangeMappingAttribute.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEAttribute(getExchangeMappingAttribute_Updatable(), ecorePackage.getEBoolean(), "updatable", null, 0, 1,
				ExchangeMappingAttribute.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEAttribute(getExchangeMappingAttribute_SelectionListStatement(), ecorePackage.getEString(), "selectionListStatement",
				null, 0, 1, ExchangeMappingAttribute.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEAttribute(getExchangeMappingAttribute_DeleteAllItems(), ecorePackage.getEBoolean(), "deleteAllItems", null, 0, 1,
				ExchangeMappingAttribute.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEAttribute(getExchangeMappingAttribute_AddNewItems(), ecorePackage.getEBoolean(), "addNewItems", null, 0, 1,
				ExchangeMappingAttribute.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEAttribute(getExchangeMappingAttribute_JoinAttribute(), ecorePackage.getEBoolean(), "joinAttribute", null, 0, 1,
				ExchangeMappingAttribute.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEAttribute(getExchangeMappingAttribute_UpdateExistingItems(), ecorePackage.getEBoolean(), "updateExistingItems", null, 0,
				1, ExchangeMappingAttribute.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);

		initEClass(filterMethodParameterEClass, FilterMethodParameter.class, "FilterMethodParameter", !IS_ABSTRACT, !IS_INTERFACE,
				IS_GENERATED_INSTANCE_CLASS);
		initEReference(getFilterMethodParameter_DomainAttribute(), theDomainPackage.getDomainAttribute(), null, "domainAttribute",
				null, 0, 1, FilterMethodParameter.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES,
				!IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getFilterMethodParameter_Association(), theDomainPackage.getAbstractDomainAssociation(), null, "association",
				null, 0, 1, FilterMethodParameter.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES,
				!IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getFilterMethodParameter_AssociationList(), theDomainPackage.getAbstractDomainAssociation(), null,
				"associationList", null, 0, -1, FilterMethodParameter.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE,
				IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getFilterMethodParameter_Operator(), ecorePackage.getEString(), "operator", null, 0, 1,
				FilterMethodParameter.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);

		initEClass(associationControllerEClass, AssociationController.class, "AssociationController", !IS_ABSTRACT, !IS_INTERFACE,
				IS_GENERATED_INSTANCE_CLASS);
		initEReference(getAssociationController_QueryAttributes(), this.getExchangeMappingAttribute(), null, "queryAttributes", null,
				0, -1, AssociationController.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES,
				!IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getAssociationController_Association(), theDomainPackage.getAbstractDomainAssociation(), null, "association",
				null, 0, 1, AssociationController.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES,
				!IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getAssociationController_PersistAttributes(), this.getExchangeMappingAttribute(), null, "persistAttributes",
				null, 0, -1, AssociationController.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES,
				!IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(directExchangeModeEClass, DirectExchangeMode.class, "DirectExchangeMode", !IS_ABSTRACT, !IS_INTERFACE,
				IS_GENERATED_INSTANCE_CLASS);

		// Initialize enums and add enum literals
		initEEnum(dataExchangeMethodTypeEnumerationEEnum, DataExchangeMethodTypeEnumeration.class,
				"DataExchangeMethodTypeEnumeration");
		addEEnumLiteral(dataExchangeMethodTypeEnumerationEEnum, DataExchangeMethodTypeEnumeration.IMPORT);
		addEEnumLiteral(dataExchangeMethodTypeEnumerationEEnum, DataExchangeMethodTypeEnumeration.EXPORT);

		initEEnum(contentTypeEnumerationEEnum, ContentTypeEnumeration.class, "ContentTypeEnumeration");
		addEEnumLiteral(contentTypeEnumerationEEnum, ContentTypeEnumeration.XML);
		addEEnumLiteral(contentTypeEnumerationEEnum, ContentTypeEnumeration.EXCEL97);
		addEEnumLiteral(contentTypeEnumerationEEnum, ContentTypeEnumeration.EXCEL2007);
		addEEnumLiteral(contentTypeEnumerationEEnum, ContentTypeEnumeration.CSV);
		addEEnumLiteral(contentTypeEnumerationEEnum, ContentTypeEnumeration.JSON);

		initEEnum(parserImplementationEnumerationEEnum, ParserImplementationEnumeration.class, "ParserImplementationEnumeration");
		addEEnumLiteral(parserImplementationEnumerationEEnum, ParserImplementationEnumeration.POI);
		addEEnumLiteral(parserImplementationEnumerationEEnum, ParserImplementationEnumeration.JAXB);
		addEEnumLiteral(parserImplementationEnumerationEEnum, ParserImplementationEnumeration.APACHE_COMMONS);
		addEEnumLiteral(parserImplementationEnumerationEEnum, ParserImplementationEnumeration.JSONB);

		// Create resource
		createResource(eNS_URI);
	}

}
