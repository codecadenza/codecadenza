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
package net.codecadenza.eclipse.model.testing.impl;

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
import net.codecadenza.eclipse.model.testing.AbstractTestCase;
import net.codecadenza.eclipse.model.testing.AbstractTestModule;
import net.codecadenza.eclipse.model.testing.GUITestAction;
import net.codecadenza.eclipse.model.testing.GUITestActionResult;
import net.codecadenza.eclipse.model.testing.GUITestActionResultComponentType;
import net.codecadenza.eclipse.model.testing.GUITestActionStatus;
import net.codecadenza.eclipse.model.testing.GUITestActionType;
import net.codecadenza.eclipse.model.testing.GUITestCase;
import net.codecadenza.eclipse.model.testing.GUITestData;
import net.codecadenza.eclipse.model.testing.GUITestDataType;
import net.codecadenza.eclipse.model.testing.SeleniumDriver;
import net.codecadenza.eclipse.model.testing.SeleniumTestModule;
import net.codecadenza.eclipse.model.testing.TestSuite;
import net.codecadenza.eclipse.model.testing.TestingFactory;
import net.codecadenza.eclipse.model.testing.TestingPackage;
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
public class TestingPackageImpl extends EPackageImpl implements TestingPackage {
	/**
	 * @generated
	 */
	private EClass abstractTestModuleEClass;

	/**
	 * @generated
	 */
	private EClass seleniumTestModuleEClass;

	/**
	 * @generated
	 */
	private EClass testSuiteEClass;

	/**
	 * @generated
	 */
	private EClass abstractTestCaseEClass;

	/**
	 * @generated
	 */
	private EClass guiTestCaseEClass;

	/**
	 * @generated
	 */
	private EClass guiTestActionEClass;

	/**
	 * @generated
	 */
	private EClass guiTestActionResultEClass;

	/**
	 * @generated
	 */
	private EClass guiTestDataEClass;

	/**
	 * @generated
	 */
	private EEnum guiTestActionStatusEEnum;

	/**
	 * @generated
	 */
	private EEnum guiTestActionTypeEEnum;

	/**
	 * @generated
	 */
	private EEnum guiTestDataTypeEEnum;

	/**
	 * @generated
	 */
	private EEnum seleniumDriverEEnum;

	/**
	 * @generated
	 */
	private EEnum guiTestActionResultComponentTypeEEnum;

	/**
	 * Create an instance of the model <b>Package</b>, registered with {@link org.eclipse.emf.ecore.EPackage.Registry
	 * EPackage.Registry} by the package package URI value.
	 * <p>
	 * Note: the correct way to create the package is via the static factory method {@link #init init()}, which also performs
	 * initialization of the package, or returns the registered package, if one already exists.
	 * @see org.eclipse.emf.ecore.EPackage.Registry
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#eNS_URI
	 * @see #init()
	 * @generated
	 */
	private TestingPackageImpl() {
		super(eNS_URI, TestingFactory.eINSTANCE);
	}

	/**
	 * @generated
	 */
	private static boolean isInited;

	/**
	 * Create, register and initialize the <b>Package</b> for this model, and for any others upon which it depends.
	 * <p>
	 * This method is used to initialize {@link TestingPackage#eINSTANCE} when that field is accessed. Clients should not invoke it
	 * directly. Instead, they should simply access that field to obtain the package.
	 * @return the package
	 * @see #eNS_URI
	 * @see #createPackageContents()
	 * @see #initializePackageContents()
	 * @generated
	 */
	public static TestingPackage init() {
		if (isInited)
			return (TestingPackage) EPackage.Registry.INSTANCE.getEPackage(TestingPackage.eNS_URI);

		// Obtain or create and register package
		final var theTestingPackage = (TestingPackageImpl) (EPackage.Registry.INSTANCE.get(eNS_URI) instanceof TestingPackageImpl
				? EPackage.Registry.INSTANCE.get(eNS_URI) : new TestingPackageImpl());

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
		theTestingPackage.createPackageContents();
		theBoundaryPackage.createPackageContents();
		theClientPackage.createPackageContents();
		theRepositoryPackage.createPackageContents();
		theDbPackage.createPackageContents();
		theDomainPackage.createPackageContents();
		theDtoPackage.createPackageContents();
		theExchangePackage.createPackageContents();
		theIntegrationPackage.createPackageContents();
		theJavaPackage.createPackageContents();
		theMappingPackage.createPackageContents();
		theProjectPackage.createPackageContents();
		theServicePackage.createPackageContents();

		// Initialize created meta-data
		theTestingPackage.initializePackageContents();
		theBoundaryPackage.initializePackageContents();
		theClientPackage.initializePackageContents();
		theRepositoryPackage.initializePackageContents();
		theDbPackage.initializePackageContents();
		theDomainPackage.initializePackageContents();
		theDtoPackage.initializePackageContents();
		theExchangePackage.initializePackageContents();
		theIntegrationPackage.initializePackageContents();
		theJavaPackage.initializePackageContents();
		theMappingPackage.initializePackageContents();
		theProjectPackage.initializePackageContents();
		theServicePackage.initializePackageContents();

		// Mark meta-data to indicate it can't be changed
		theTestingPackage.freeze();

		// Update the registry and return the package
		EPackage.Registry.INSTANCE.put(TestingPackage.eNS_URI, theTestingPackage);
		return theTestingPackage;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getAbstractTestModule()
	 * @generated
	 */
	@Override
	public EClass getAbstractTestModule() {
		return abstractTestModuleEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getAbstractTestModule_Namespace()
	 * @generated
	 */
	@Override
	public EReference getAbstractTestModule_Namespace() {
		return (EReference) abstractTestModuleEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getAbstractTestModule_TestCaseSuffix()
	 * @generated
	 */
	@Override
	public EAttribute getAbstractTestModule_TestCaseSuffix() {
		return (EAttribute) abstractTestModuleEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getAbstractTestModule_Project()
	 * @generated
	 */
	@Override
	public EReference getAbstractTestModule_Project() {
		return (EReference) abstractTestModuleEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getSeleniumTestModule()
	 * @generated
	 */
	@Override
	public EClass getSeleniumTestModule() {
		return seleniumTestModuleEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getSeleniumTestModule_Driver()
	 * @generated
	 */
	@Override
	public EAttribute getSeleniumTestModule_Driver() {
		return (EAttribute) seleniumTestModuleEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getSeleniumTestModule_DriverPath()
	 * @generated
	 */
	@Override
	public EAttribute getSeleniumTestModule_DriverPath() {
		return (EAttribute) seleniumTestModuleEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getSeleniumTestModule_MaximizeWindow()
	 * @generated
	 */
	@Override
	public EAttribute getSeleniumTestModule_MaximizeWindow() {
		return (EAttribute) seleniumTestModuleEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getSeleniumTestModule_ImplicitWaitTime()
	 * @generated
	 */
	@Override
	public EAttribute getSeleniumTestModule_ImplicitWaitTime() {
		return (EAttribute) seleniumTestModuleEClass.getEStructuralFeatures().get(3);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getSeleniumTestModule_PageLoadTime()
	 * @generated
	 */
	@Override
	public EAttribute getSeleniumTestModule_PageLoadTime() {
		return (EAttribute) seleniumTestModuleEClass.getEStructuralFeatures().get(4);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getTestSuite()
	 * @generated
	 */
	@Override
	public EClass getTestSuite() {
		return testSuiteEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getTestSuite_TestCases()
	 * @generated
	 */
	@Override
	public EReference getTestSuite_TestCases() {
		return (EReference) testSuiteEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getAbstractTestCase()
	 * @generated
	 */
	@Override
	public EClass getAbstractTestCase() {
		return abstractTestCaseEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestCase()
	 * @generated
	 */
	@Override
	public EClass getGUITestCase() {
		return guiTestCaseEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestCase_TestActions()
	 * @generated
	 */
	@Override
	public EReference getGUITestCase_TestActions() {
		return (EReference) guiTestCaseEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestAction()
	 * @generated
	 */
	@Override
	public EClass getGUITestAction() {
		return guiTestActionEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestAction_Comment()
	 * @generated
	 */
	@Override
	public EAttribute getGUITestAction_Comment() {
		return (EAttribute) guiTestActionEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestAction_Form()
	 * @generated
	 */
	@Override
	public EReference getGUITestAction_Form() {
		return (EReference) guiTestActionEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestAction_TargetForm()
	 * @generated
	 */
	@Override
	public EReference getGUITestAction_TargetForm() {
		return (EReference) guiTestActionEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestAction_FormAction()
	 * @generated
	 */
	@Override
	public EReference getGUITestAction_FormAction() {
		return (EReference) guiTestActionEClass.getEStructuralFeatures().get(3);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestAction_FormPanel()
	 * @generated
	 */
	@Override
	public EReference getGUITestAction_FormPanel() {
		return (EReference) guiTestActionEClass.getEStructuralFeatures().get(4);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestAction_Type()
	 * @generated
	 */
	@Override
	public EAttribute getGUITestAction_Type() {
		return (EAttribute) guiTestActionEClass.getEStructuralFeatures().get(5);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestAction_TestData()
	 * @generated
	 */
	@Override
	public EReference getGUITestAction_TestData() {
		return (EReference) guiTestActionEClass.getEStructuralFeatures().get(6);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestAction_ActionResult()
	 * @generated
	 */
	@Override
	public EReference getGUITestAction_ActionResult() {
		return (EReference) guiTestActionEClass.getEStructuralFeatures().get(7);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestAction_TestCase()
	 * @generated
	 */
	@Override
	public EReference getGUITestAction_TestCase() {
		return (EReference) guiTestActionEClass.getEStructuralFeatures().get(8);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestAction_DelayBefore()
	 * @generated
	 */
	@Override
	public EAttribute getGUITestAction_DelayBefore() {
		return (EAttribute) guiTestActionEClass.getEStructuralFeatures().get(9);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestAction_DelayAfter()
	 * @generated
	 */
	@Override
	public EAttribute getGUITestAction_DelayAfter() {
		return (EAttribute) guiTestActionEClass.getEStructuralFeatures().get(10);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestActionResult()
	 * @generated
	 */
	@Override
	public EClass getGUITestActionResult() {
		return guiTestActionResultEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestActionResult_MessageText()
	 * @generated
	 */
	@Override
	public EAttribute getGUITestActionResult_MessageText() {
		return (EAttribute) guiTestActionResultEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestActionResult_Status()
	 * @generated
	 */
	@Override
	public EAttribute getGUITestActionResult_Status() {
		return (EAttribute) guiTestActionResultEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestActionResult_TestAction()
	 * @generated
	 */
	@Override
	public EReference getGUITestActionResult_TestAction() {
		return (EReference) guiTestActionResultEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestActionResult_ComponentType()
	 * @generated
	 */
	@Override
	public EAttribute getGUITestActionResult_ComponentType() {
		return (EAttribute) guiTestActionResultEClass.getEStructuralFeatures().get(3);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestData()
	 * @generated
	 */
	@Override
	public EClass getGUITestData() {
		return guiTestDataEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestData_FormField()
	 * @generated
	 */
	@Override
	public EReference getGUITestData_FormField() {
		return (EReference) guiTestDataEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestData_TableColumnField()
	 * @generated
	 */
	@Override
	public EReference getGUITestData_TableColumnField() {
		return (EReference) guiTestDataEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestData_NewValue()
	 * @generated
	 */
	@Override
	public EAttribute getGUITestData_NewValue() {
		return (EAttribute) guiTestDataEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestData_ExpectedValue()
	 * @generated
	 */
	@Override
	public EAttribute getGUITestData_ExpectedValue() {
		return (EAttribute) guiTestDataEClass.getEStructuralFeatures().get(3);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestData_Type()
	 * @generated
	 */
	@Override
	public EAttribute getGUITestData_Type() {
		return (EAttribute) guiTestDataEClass.getEStructuralFeatures().get(4);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestData_TestAction()
	 * @generated
	 */
	@Override
	public EReference getGUITestData_TestAction() {
		return (EReference) guiTestDataEClass.getEStructuralFeatures().get(5);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestData_FilterValue()
	 */
	@Override
	public EAttribute getGUITestData_FilterValue() {
		return (EAttribute) guiTestDataEClass.getEStructuralFeatures().get(6);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestActionStatus()
	 * @generated
	 */
	@Override
	public EEnum getGUITestActionStatus() {
		return guiTestActionStatusEEnum;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestActionType()
	 * @generated
	 */
	@Override
	public EEnum getGUITestActionType() {
		return guiTestActionTypeEEnum;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestDataType()
	 * @generated
	 */
	@Override
	public EEnum getGUITestDataType() {
		return guiTestDataTypeEEnum;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getSeleniumDriver()
	 * @generated
	 */
	@Override
	public EEnum getSeleniumDriver() {
		return seleniumDriverEEnum;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getGUITestActionResultComponentType()
	 * @generated
	 */
	@Override
	public EEnum getGUITestActionResultComponentType() {
		return guiTestActionResultComponentTypeEEnum;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getTestingFactory()
	 * @generated
	 */
	@Override
	public TestingFactory getTestingFactory() {
		return (TestingFactory) getEFactoryInstance();
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
		abstractTestModuleEClass = createEClass(ABSTRACT_TEST_MODULE);
		createEReference(abstractTestModuleEClass, ABSTRACT_TEST_MODULE__NAMESPACE);
		createEAttribute(abstractTestModuleEClass, ABSTRACT_TEST_MODULE__TEST_CASE_SUFFIX);
		createEReference(abstractTestModuleEClass, ABSTRACT_TEST_MODULE__PROJECT);

		seleniumTestModuleEClass = createEClass(SELENIUM_TEST_MODULE);
		createEAttribute(seleniumTestModuleEClass, SELENIUM_TEST_MODULE__DRIVER);
		createEAttribute(seleniumTestModuleEClass, SELENIUM_TEST_MODULE__DRIVER_PATH);
		createEAttribute(seleniumTestModuleEClass, SELENIUM_TEST_MODULE__MAXIMIZE_WINDOW);
		createEAttribute(seleniumTestModuleEClass, SELENIUM_TEST_MODULE__IMPLICIT_WAIT_TIME);
		createEAttribute(seleniumTestModuleEClass, SELENIUM_TEST_MODULE__PAGE_LOAD_TIME);

		testSuiteEClass = createEClass(TEST_SUITE);
		createEReference(testSuiteEClass, TEST_SUITE__TEST_CASES);

		abstractTestCaseEClass = createEClass(ABSTRACT_TEST_CASE);

		guiTestCaseEClass = createEClass(GUI_TEST_CASE);
		createEReference(guiTestCaseEClass, GUI_TEST_CASE__TEST_ACTIONS);

		guiTestActionEClass = createEClass(GUI_TEST_ACTION);
		createEAttribute(guiTestActionEClass, GUI_TEST_ACTION__COMMENT);
		createEReference(guiTestActionEClass, GUI_TEST_ACTION__FORM);
		createEReference(guiTestActionEClass, GUI_TEST_ACTION__TARGET_FORM);
		createEReference(guiTestActionEClass, GUI_TEST_ACTION__FORM_ACTION);
		createEReference(guiTestActionEClass, GUI_TEST_ACTION__FORM_PANEL);
		createEAttribute(guiTestActionEClass, GUI_TEST_ACTION__TYPE);
		createEReference(guiTestActionEClass, GUI_TEST_ACTION__TEST_DATA);
		createEReference(guiTestActionEClass, GUI_TEST_ACTION__ACTION_RESULT);
		createEReference(guiTestActionEClass, GUI_TEST_ACTION__TEST_CASE);
		createEAttribute(guiTestActionEClass, GUI_TEST_ACTION__DELAY_BEFORE);
		createEAttribute(guiTestActionEClass, GUI_TEST_ACTION__DELAY_AFTER);

		guiTestActionResultEClass = createEClass(GUI_TEST_ACTION_RESULT);
		createEAttribute(guiTestActionResultEClass, GUI_TEST_ACTION_RESULT__MESSAGE_TEXT);
		createEAttribute(guiTestActionResultEClass, GUI_TEST_ACTION_RESULT__STATUS);
		createEReference(guiTestActionResultEClass, GUI_TEST_ACTION_RESULT__TEST_ACTION);
		createEAttribute(guiTestActionResultEClass, GUI_TEST_ACTION_RESULT__COMPONENT_TYPE);

		guiTestDataEClass = createEClass(GUI_TEST_DATA);
		createEReference(guiTestDataEClass, GUI_TEST_DATA__FORM_FIELD);
		createEReference(guiTestDataEClass, GUI_TEST_DATA__TABLE_COLUMN_FIELD);
		createEAttribute(guiTestDataEClass, GUI_TEST_DATA__NEW_VALUE);
		createEAttribute(guiTestDataEClass, GUI_TEST_DATA__EXPECTED_VALUE);
		createEAttribute(guiTestDataEClass, GUI_TEST_DATA__TYPE);
		createEReference(guiTestDataEClass, GUI_TEST_DATA__TEST_ACTION);
		createEAttribute(guiTestDataEClass, GUI_TEST_DATA__FILTER_VALUE);

		// Create enums
		guiTestActionStatusEEnum = createEEnum(GUI_TEST_ACTION_STATUS);
		guiTestActionTypeEEnum = createEEnum(GUI_TEST_ACTION_TYPE);
		guiTestDataTypeEEnum = createEEnum(GUI_TEST_DATA_TYPE);
		seleniumDriverEEnum = createEEnum(SELENIUM_DRIVER);
		guiTestActionResultComponentTypeEEnum = createEEnum(GUI_TEST_ACTION_RESULT_COMPONENT_TYPE);
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
		final var theJavaPackage = (JavaPackage) EPackage.Registry.INSTANCE.getEPackage(JavaPackage.eNS_URI);
		final var theProjectPackage = (ProjectPackage) EPackage.Registry.INSTANCE.getEPackage(ProjectPackage.eNS_URI);
		final var theClientPackage = (ClientPackage) EPackage.Registry.INSTANCE.getEPackage(ClientPackage.eNS_URI);

		// Add supertypes to classes
		seleniumTestModuleEClass.getESuperTypes().add(this.getAbstractTestModule());
		testSuiteEClass.getESuperTypes().add(theJavaPackage.getJavaType());
		abstractTestCaseEClass.getESuperTypes().add(theJavaPackage.getJavaType());
		guiTestCaseEClass.getESuperTypes().add(this.getAbstractTestCase());

		// Initialize classes and features; add operations and parameters
		initEClass(abstractTestModuleEClass, AbstractTestModule.class, "AbstractTestModule", IS_ABSTRACT, !IS_INTERFACE,
				IS_GENERATED_INSTANCE_CLASS);
		initEReference(getAbstractTestModule_Namespace(), theJavaPackage.getNamespace(), null, "namespace", null, 0, 1,
				AbstractTestModule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE,
				IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getAbstractTestModule_TestCaseSuffix(), ecorePackage.getEString(), "testCaseSuffix", null, 0, 1,
				AbstractTestModule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEReference(getAbstractTestModule_Project(), theProjectPackage.getProject(), theProjectPackage.getProject_TestModules(),
				"project", null, 0, 1, AbstractTestModule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE,
				!IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		addEOperation(abstractTestModuleEClass, this.getTestSuite(), "getTestSuites", 0, -1, IS_UNIQUE, IS_ORDERED);

		addEOperation(abstractTestModuleEClass, this.getAbstractTestCase(), "getTestCases", 0, -1, IS_UNIQUE, IS_ORDERED);

		initEClass(seleniumTestModuleEClass, SeleniumTestModule.class, "SeleniumTestModule", !IS_ABSTRACT, !IS_INTERFACE,
				IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getSeleniumTestModule_Driver(), this.getSeleniumDriver(), "driver", null, 0, 1, SeleniumTestModule.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getSeleniumTestModule_DriverPath(), ecorePackage.getEString(), "driverPath", null, 0, 1,
				SeleniumTestModule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getSeleniumTestModule_MaximizeWindow(), ecorePackage.getEBoolean(), "maximizeWindow", null, 0, 1,
				SeleniumTestModule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getSeleniumTestModule_ImplicitWaitTime(), ecorePackage.getEInt(), "implicitWaitTime", null, 0, 1,
				SeleniumTestModule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getSeleniumTestModule_PageLoadTime(), ecorePackage.getEInt(), "pageLoadTime", null, 0, 1,
				SeleniumTestModule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);

		initEClass(testSuiteEClass, TestSuite.class, "TestSuite", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getTestSuite_TestCases(), this.getAbstractTestCase(), null, "testCases", null, 0, -1, TestSuite.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);

		initEClass(abstractTestCaseEClass, AbstractTestCase.class, "AbstractTestCase", IS_ABSTRACT, !IS_INTERFACE,
				IS_GENERATED_INSTANCE_CLASS);

		initEClass(guiTestCaseEClass, GUITestCase.class, "GUITestCase", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getGUITestCase_TestActions(), this.getGUITestAction(), this.getGUITestAction_TestCase(), "testActions", null,
				0, -1, GUITestCase.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE,
				IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(guiTestActionEClass, GUITestAction.class, "GUITestAction", !IS_ABSTRACT, !IS_INTERFACE,
				IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getGUITestAction_Comment(), ecorePackage.getEString(), "comment", null, 0, 1, GUITestAction.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getGUITestAction_Form(), theClientPackage.getForm(), null, "form", null, 0, 1, GUITestAction.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEReference(getGUITestAction_TargetForm(), theClientPackage.getForm(), null, "targetForm", null, 0, 1, GUITestAction.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEReference(getGUITestAction_FormAction(), theClientPackage.getFormAction(), null, "formAction", null, 0, 1,
				GUITestAction.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE,
				IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getGUITestAction_FormPanel(), theClientPackage.getFormPanel(), null, "formPanel", null, 0, 1,
				GUITestAction.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE,
				IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getGUITestAction_Type(), this.getGUITestActionType(), "type", null, 0, 1, GUITestAction.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getGUITestAction_TestData(), this.getGUITestData(), this.getGUITestData_TestAction(), "testData", null, 0, -1,
				GUITestAction.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE,
				IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getGUITestAction_ActionResult(), this.getGUITestActionResult(), this.getGUITestActionResult_TestAction(),
				"actionResult", null, 0, 1, GUITestAction.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE,
				!IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getGUITestAction_TestCase(), this.getGUITestCase(), this.getGUITestCase_TestActions(), "testCase", null, 0, 1,
				GUITestAction.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE,
				IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getGUITestAction_DelayBefore(), ecorePackage.getEIntegerObject(), "delayBefore", null, 0, 1,
				GUITestAction.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getGUITestAction_DelayAfter(), ecorePackage.getEIntegerObject(), "delayAfter", null, 0, 1, GUITestAction.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(guiTestActionResultEClass, GUITestActionResult.class, "GUITestActionResult", !IS_ABSTRACT, !IS_INTERFACE,
				IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getGUITestActionResult_MessageText(), ecorePackage.getEString(), "messageText", null, 0, 1,
				GUITestActionResult.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getGUITestActionResult_Status(), this.getGUITestActionStatus(), "status", null, 0, 1,
				GUITestActionResult.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEReference(getGUITestActionResult_TestAction(), this.getGUITestAction(), this.getGUITestAction_ActionResult(),
				"testAction", null, 0, 1, GUITestActionResult.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE,
				!IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getGUITestActionResult_ComponentType(), this.getGUITestActionResultComponentType(), "componentType", null, 0,
				1, GUITestActionResult.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);

		initEClass(guiTestDataEClass, GUITestData.class, "GUITestData", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getGUITestData_FormField(), theClientPackage.getFormField(), null, "formField", null, 0, 1, GUITestData.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEReference(getGUITestData_TableColumnField(), theClientPackage.getTableColumnField(), null, "tableColumnField", null, 0,
				1, GUITestData.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE,
				IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getGUITestData_NewValue(), ecorePackage.getEString(), "newValue", null, 0, 1, GUITestData.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getGUITestData_ExpectedValue(), ecorePackage.getEString(), "expectedValue", null, 0, 1, GUITestData.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getGUITestData_Type(), this.getGUITestDataType(), "type", null, 0, 1, GUITestData.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getGUITestData_TestAction(), this.getGUITestAction(), this.getGUITestAction_TestData(), "testAction", null, 0,
				1, GUITestData.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE,
				IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getGUITestData_FilterValue(), ecorePackage.getEString(), "filterValue", null, 0, 1, GUITestData.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		// Initialize enums and add enum literals
		initEEnum(guiTestActionStatusEEnum, GUITestActionStatus.class, "GUITestActionStatus");
		addEEnumLiteral(guiTestActionStatusEEnum, GUITestActionStatus.INFO);
		addEEnumLiteral(guiTestActionStatusEEnum, GUITestActionStatus.WARNING);
		addEEnumLiteral(guiTestActionStatusEEnum, GUITestActionStatus.ERROR);

		initEEnum(guiTestActionTypeEEnum, GUITestActionType.class, "GUITestActionType");
		addEEnumLiteral(guiTestActionTypeEEnum, GUITestActionType.EXECUTE_FORM_ACTION);
		addEEnumLiteral(guiTestActionTypeEEnum, GUITestActionType.EXECUTE_REFRESH_ACTION);
		addEEnumLiteral(guiTestActionTypeEEnum, GUITestActionType.OPEN_PAGE_DIRECT);
		addEEnumLiteral(guiTestActionTypeEEnum, GUITestActionType.OPEN_PAGE_BY_NAVIGATOR);
		addEEnumLiteral(guiTestActionTypeEEnum, GUITestActionType.PERFORM_LOGOUT);
		addEEnumLiteral(guiTestActionTypeEEnum, GUITestActionType.SEARCH_ROW_CURRENT_PAGE);
		addEEnumLiteral(guiTestActionTypeEEnum, GUITestActionType.SEARCH_ROW_ALL_PAGES);
		addEEnumLiteral(guiTestActionTypeEEnum, GUITestActionType.ENTER_SEARCH_DATA);
		addEEnumLiteral(guiTestActionTypeEEnum, GUITestActionType.COUNT_RECORDS);
		addEEnumLiteral(guiTestActionTypeEEnum, GUITestActionType.RESET_SEARCH_DATA);
		addEEnumLiteral(guiTestActionTypeEEnum, GUITestActionType.ENTER_FORM_DATA);
		addEEnumLiteral(guiTestActionTypeEEnum, GUITestActionType.VALIDATE_FORM_DATA);
		addEEnumLiteral(guiTestActionTypeEEnum, GUITestActionType.DOUBLE_CLICK_ROW);
		addEEnumLiteral(guiTestActionTypeEEnum, GUITestActionType.PRESS_OK_BUTTON);
		addEEnumLiteral(guiTestActionTypeEEnum, GUITestActionType.PRESS_CANCEL_BUTTON);
		addEEnumLiteral(guiTestActionTypeEEnum, GUITestActionType.OPEN_LOGIN_PAGE);
		addEEnumLiteral(guiTestActionTypeEEnum, GUITestActionType.UPLOAD_FILE);
		addEEnumLiteral(guiTestActionTypeEEnum, GUITestActionType.PRESS_DOWNLOAD_BUTTON);
		addEEnumLiteral(guiTestActionTypeEEnum, GUITestActionType.VALIDATE_ROW_COUNT_EQUAL);
		addEEnumLiteral(guiTestActionTypeEEnum, GUITestActionType.VALIDATE_ROW_COUNT_GREATER);
		addEEnumLiteral(guiTestActionTypeEEnum, GUITestActionType.VALIDATE_ROW_COUNT_SMALLER);

		initEEnum(guiTestDataTypeEEnum, GUITestDataType.class, "GUITestDataType");
		addEEnumLiteral(guiTestDataTypeEEnum, GUITestDataType.FORM_FIELD);
		addEEnumLiteral(guiTestDataTypeEEnum, GUITestDataType.PAGE_TITLE);
		addEEnumLiteral(guiTestDataTypeEEnum, GUITestDataType.OBJECT_ID);
		addEEnumLiteral(guiTestDataTypeEEnum, GUITestDataType.ROW_INDEX);
		addEEnumLiteral(guiTestDataTypeEEnum, GUITestDataType.CELL_VALUE);
		addEEnumLiteral(guiTestDataTypeEEnum, GUITestDataType.ROW_COUNT);
		addEEnumLiteral(guiTestDataTypeEEnum, GUITestDataType.SEARCH_FILTER);
		addEEnumLiteral(guiTestDataTypeEEnum, GUITestDataType.SEARCH_OPERATOR);
		addEEnumLiteral(guiTestDataTypeEEnum, GUITestDataType.SEARCH_SORT_ORDER);

		initEEnum(seleniumDriverEEnum, SeleniumDriver.class, "SeleniumDriver");
		addEEnumLiteral(seleniumDriverEEnum, SeleniumDriver.CHROME);
		addEEnumLiteral(seleniumDriverEEnum, SeleniumDriver.EDGE);
		addEEnumLiteral(seleniumDriverEEnum, SeleniumDriver.FIREFOX);
		addEEnumLiteral(seleniumDriverEEnum, SeleniumDriver.INTERNET_EXPLORER);
		addEEnumLiteral(seleniumDriverEEnum, SeleniumDriver.OPERA);
		addEEnumLiteral(seleniumDriverEEnum, SeleniumDriver.SAFARI);

		initEEnum(guiTestActionResultComponentTypeEEnum, GUITestActionResultComponentType.class, "GUITestActionResultComponentType");
		addEEnumLiteral(guiTestActionResultComponentTypeEEnum, GUITestActionResultComponentType.DIALOG);
		addEEnumLiteral(guiTestActionResultComponentTypeEEnum, GUITestActionResultComponentType.NOTIFICATION);

		// Create resource
		createResource(eNS_URI);
	}

}
