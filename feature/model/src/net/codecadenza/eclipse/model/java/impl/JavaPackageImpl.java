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
package net.codecadenza.eclipse.model.java.impl;

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
import net.codecadenza.eclipse.model.java.EnumLiteral;
import net.codecadenza.eclipse.model.java.EnumLiteralTagEnumeration;
import net.codecadenza.eclipse.model.java.EnumTagEnumeration;
import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.model.java.JavaFactory;
import net.codecadenza.eclipse.model.java.JavaMethod;
import net.codecadenza.eclipse.model.java.JavaPackage;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.JavaTypeModifierEnumeration;
import net.codecadenza.eclipse.model.java.MethodParameter;
import net.codecadenza.eclipse.model.java.Namespace;
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
 * The <b>Package</b> for the model. It contains accessors for the meta objects to represent
 * <ul>
 * <li>each class,</li>
 * <li>each feature of each class,</li>
 * <li>each enum,</li>
 * <li>and each data type</li>
 * </ul>
 * @generated
 */
public class JavaPackageImpl extends EPackageImpl implements JavaPackage {
	/**
	 * @generated
	 */
	private EClass enumLiteralEClass;

	/**
	 * @generated
	 */
	private EClass javaEnumEClass;

	/**
	 * @generated
	 */
	private EClass javaMethodEClass;

	/**
	 * @generated
	 */
	private EClass javaTypeEClass;

	/**
	 * @generated
	 */
	private EClass methodParameterEClass;

	/**
	 * @generated
	 */
	private EClass namespaceEClass;

	/**
	 * @generated
	 */
	private EEnum javaTypeModifierEnumerationEEnum;

	/**
	 * @generated
	 */
	private EEnum enumLiteralTagEnumerationEEnum;

	/**
	 * @generated
	 */
	private EEnum enumTagEnumerationEEnum;

	/**
	 * Create an instance of the model <b>Package</b>, registered with {@link org.eclipse.emf.ecore.EPackage.Registry
	 * EPackage.Registry} by the package URI value.
	 * <p>
	 * Note: The correct way to create the package is via the static factory method {@link #init init()}, which also performs
	 * initialization of the package, or returns the registered package, if one already exists.
	 * </p>
	 * @see org.eclipse.emf.ecore.EPackage.Registry
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#eNS_URI
	 * @see #init()
	 * @generated
	 */
	private JavaPackageImpl() {
		super(eNS_URI, JavaFactory.eINSTANCE);
	}

	/**
	 * @generated
	 */
	private static boolean isInited;

	/**
	 * Create, register and initialize the <b>Package</b> for this model, and for any others upon which it depends. This method is
	 * used to initialize {@link JavaPackage#eINSTANCE} when that field is accessed. Clients should not invoke it directly. Instead,
	 * they should simply access that field to obtain the package.
	 * @see #eNS_URI
	 * @see #createPackageContents()
	 * @see #initializePackageContents()
	 * @return the initialized Java package
	 * @generated
	 */
	public static JavaPackage init() {
		if (isInited)
			return (JavaPackage) EPackage.Registry.INSTANCE.getEPackage(JavaPackage.eNS_URI);

		// Obtain or create and register package
		final var theJavaPackage = (JavaPackageImpl) (EPackage.Registry.INSTANCE.get(eNS_URI) instanceof JavaPackageImpl
				? EPackage.Registry.INSTANCE.get(eNS_URI) : new JavaPackageImpl());

		isInited = true;

		// Obtain or create and register interdependencies
		final var theBoundaryPackage = (BoundaryPackageImpl) (EPackage.Registry.INSTANCE
				.getEPackage(BoundaryPackage.eNS_URI) instanceof BoundaryPackageImpl
						? EPackage.Registry.INSTANCE.getEPackage(BoundaryPackage.eNS_URI) : BoundaryPackage.eINSTANCE);
		final var theClientPackage = (ClientPackageImpl) (EPackage.Registry.INSTANCE
				.getEPackage(ClientPackage.eNS_URI) instanceof ClientPackageImpl
						? EPackage.Registry.INSTANCE.getEPackage(ClientPackage.eNS_URI) : ClientPackage.eINSTANCE);
		final var theRepositoryNamespace = (RepositoryPackageImpl) (EPackage.Registry.INSTANCE
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
		final var theProjectPackage = (ProjectPackageImpl) (EPackage.Registry.INSTANCE
				.getEPackage(ProjectPackage.eNS_URI) instanceof ProjectPackageImpl
						? EPackage.Registry.INSTANCE.getEPackage(ProjectPackage.eNS_URI) : ProjectPackage.eINSTANCE);
		final var theExchangePackage = (ExchangePackageImpl) (EPackage.Registry.INSTANCE
				.getEPackage(ExchangePackage.eNS_URI) instanceof ExchangePackageImpl
						? EPackage.Registry.INSTANCE.getEPackage(ExchangePackage.eNS_URI) : ExchangePackage.eINSTANCE);
		final var theServicePackage = (ServicePackageImpl) (EPackage.Registry.INSTANCE
				.getEPackage(ServicePackage.eNS_URI) instanceof ServicePackageImpl
						? EPackage.Registry.INSTANCE.getEPackage(ServicePackage.eNS_URI) : ServicePackage.eINSTANCE);
		final var theMappingPackage = (MappingPackageImpl) (EPackage.Registry.INSTANCE
				.getEPackage(MappingPackage.eNS_URI) instanceof MappingPackageImpl
						? EPackage.Registry.INSTANCE.getEPackage(MappingPackage.eNS_URI) : MappingPackage.eINSTANCE);

		// Create package meta-data objects
		theJavaPackage.createPackageContents();
		theBoundaryPackage.createPackageContents();
		theClientPackage.createPackageContents();
		theRepositoryNamespace.createPackageContents();
		theDbPackage.createPackageContents();
		theDomainPackage.createPackageContents();
		theDtoPackage.createPackageContents();
		theProjectPackage.createPackageContents();
		theExchangePackage.createPackageContents();
		theServicePackage.createPackageContents();
		theMappingPackage.createPackageContents();

		// Initialize created meta-data
		theJavaPackage.initializePackageContents();
		theBoundaryPackage.initializePackageContents();
		theClientPackage.initializePackageContents();
		theRepositoryNamespace.initializePackageContents();
		theDbPackage.initializePackageContents();
		theDomainPackage.initializePackageContents();
		theDtoPackage.initializePackageContents();
		theProjectPackage.initializePackageContents();
		theExchangePackage.initializePackageContents();
		theServicePackage.initializePackageContents();
		theMappingPackage.initializePackageContents();

		// Mark meta-data to indicate it can't be changed
		theJavaPackage.freeze();

		// Update the registry and return the package
		EPackage.Registry.INSTANCE.put(JavaPackage.eNS_URI, theJavaPackage);
		return theJavaPackage;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getEnumLiteral()
	 * @generated
	 */
	@Override
	public EClass getEnumLiteral() {
		return enumLiteralEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getEnumLiteral_Name()
	 * @generated
	 */
	@Override
	public EAttribute getEnumLiteral_Name() {
		return (EAttribute) enumLiteralEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getEnumLiteral_Tag()
	 * @generated
	 */
	@Override
	public EAttribute getEnumLiteral_Tag() {
		return (EAttribute) enumLiteralEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getEnumLiteral_JavaEnum()
	 * @generated
	 */
	@Override
	public EReference getEnumLiteral_JavaEnum() {
		return (EReference) enumLiteralEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getJavaEnum()
	 * @generated
	 */
	@Override
	public EClass getJavaEnum() {
		return javaEnumEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getJavaEnum_EnumerationValues()
	 * @generated
	 */
	@Override
	public EReference getJavaEnum_EnumerationValues() {
		return (EReference) javaEnumEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getJavaEnum_Tag()
	 * @generated
	 */
	@Override
	public EAttribute getJavaEnum_Tag() {
		return (EAttribute) javaEnumEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getJavaMethod()
	 * @generated
	 */
	@Override
	public EClass getJavaMethod() {
		return javaMethodEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getJavaMethod_Name()
	 * @generated
	 */
	@Override
	public EAttribute getJavaMethod_Name() {
		return (EAttribute) javaMethodEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getJavaMethod_Comment()
	 * @generated
	 */
	@Override
	public EAttribute getJavaMethod_Comment() {
		return (EAttribute) javaMethodEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getJavaMethod_JavaType()
	 * @generated
	 */
	@Override
	public EReference getJavaMethod_JavaType() {
		return (EReference) javaMethodEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getJavaMethod_ReturnType()
	 * @generated
	 */
	@Override
	public EReference getJavaMethod_ReturnType() {
		return (EReference) javaMethodEClass.getEStructuralFeatures().get(3);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getJavaMethod_ReturnTypeModifier()
	 * @generated
	 */
	@Override
	public EAttribute getJavaMethod_ReturnTypeModifier() {
		return (EAttribute) javaMethodEClass.getEStructuralFeatures().get(4);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getJavaMethod_MethodParameters()
	 * @generated
	 */
	@Override
	public EReference getJavaMethod_MethodParameters() {
		return (EReference) javaMethodEClass.getEStructuralFeatures().get(5);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getJavaType()
	 * @generated
	 */
	@Override
	public EClass getJavaType() {
		return javaTypeEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getJavaType_Name()
	 * @generated
	 */
	@Override
	public EAttribute getJavaType_Name() {
		return (EAttribute) javaTypeEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getJavaType_Comment()
	 * @generated
	 */
	@Override
	public EAttribute getJavaType_Comment() {
		return (EAttribute) javaTypeEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getJavaType_Mappable()
	 * @generated
	 */
	@Override
	public EAttribute getJavaType_Mappable() {
		return (EAttribute) javaTypeEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getJavaType_Primitive()
	 * @generated
	 */
	@Override
	public EAttribute getJavaType_Primitive() {
		return (EAttribute) javaTypeEClass.getEStructuralFeatures().get(3);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getJavaType_Namespace()
	 * @generated
	 */
	@Override
	public EReference getJavaType_Namespace() {
		return (EReference) javaTypeEClass.getEStructuralFeatures().get(4);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getMethodParameter()
	 * @generated
	 */
	@Override
	public EClass getMethodParameter() {
		return methodParameterEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getMethodParameter_Name()
	 * @generated
	 */
	@Override
	public EAttribute getMethodParameter_Name() {
		return (EAttribute) methodParameterEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getMethodParameter_Method()
	 * @generated
	 */
	@Override
	public EReference getMethodParameter_Method() {
		return (EReference) methodParameterEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getMethodParameter_Type()
	 * @generated
	 */
	@Override
	public EReference getMethodParameter_Type() {
		return (EReference) methodParameterEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getMethodParameter_Modifier()
	 * @generated
	 */
	@Override
	public EAttribute getMethodParameter_Modifier() {
		return (EAttribute) methodParameterEClass.getEStructuralFeatures().get(3);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getMethodParameter_Hint()
	 * @generated
	 */
	@Override
	public EAttribute getMethodParameter_Hint() {
		return (EAttribute) methodParameterEClass.getEStructuralFeatures().get(4);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getNamespace()
	 * @generated
	 */
	@Override
	public EClass getNamespace() {
		return namespaceEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getNamespace_Name()
	 * @generated
	 */
	@Override
	public EAttribute getNamespace_Name() {
		return (EAttribute) namespaceEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getNamespace_Parent()
	 * @generated
	 */
	@Override
	public EReference getNamespace_Parent() {
		return (EReference) namespaceEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getNamespace_ChildNamespaces()
	 * @generated
	 */
	@Override
	public EReference getNamespace_ChildNamespaces() {
		return (EReference) namespaceEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getNamespace_JavaTypes()
	 * @generated
	 */
	@Override
	public EReference getNamespace_JavaTypes() {
		return (EReference) namespaceEClass.getEStructuralFeatures().get(3);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getNamespace_Project()
	 * @generated
	 */
	@Override
	public EReference getNamespace_Project() {
		return (EReference) namespaceEClass.getEStructuralFeatures().get(4);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getJavaTypeModifierEnumeration()
	 * @generated
	 */
	@Override
	public EEnum getJavaTypeModifierEnumeration() {
		return javaTypeModifierEnumerationEEnum;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getEnumLiteralTagEnumeration()
	 * @generated
	 */
	@Override
	public EEnum getEnumLiteralTagEnumeration() {
		return enumLiteralTagEnumerationEEnum;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getEnumTagEnumeration()
	 * @generated
	 */
	@Override
	public EEnum getEnumTagEnumeration() {
		return enumTagEnumerationEEnum;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getJavaFactory()
	 * @generated
	 */
	@Override
	public JavaFactory getJavaFactory() {
		return (JavaFactory) getEFactoryInstance();
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
		enumLiteralEClass = createEClass(ENUM_LITERAL);
		createEAttribute(enumLiteralEClass, ENUM_LITERAL__NAME);
		createEAttribute(enumLiteralEClass, ENUM_LITERAL__TAG);
		createEReference(enumLiteralEClass, ENUM_LITERAL__JAVA_ENUM);

		javaEnumEClass = createEClass(JAVA_ENUM);
		createEReference(javaEnumEClass, JAVA_ENUM__ENUMERATION_VALUES);
		createEAttribute(javaEnumEClass, JAVA_ENUM__TAG);

		javaMethodEClass = createEClass(JAVA_METHOD);
		createEAttribute(javaMethodEClass, JAVA_METHOD__NAME);
		createEAttribute(javaMethodEClass, JAVA_METHOD__COMMENT);
		createEReference(javaMethodEClass, JAVA_METHOD__JAVA_TYPE);
		createEReference(javaMethodEClass, JAVA_METHOD__RETURN_TYPE);
		createEAttribute(javaMethodEClass, JAVA_METHOD__RETURN_TYPE_MODIFIER);
		createEReference(javaMethodEClass, JAVA_METHOD__METHOD_PARAMETERS);

		javaTypeEClass = createEClass(JAVA_TYPE);
		createEAttribute(javaTypeEClass, JAVA_TYPE__NAME);
		createEAttribute(javaTypeEClass, JAVA_TYPE__COMMENT);
		createEAttribute(javaTypeEClass, JAVA_TYPE__MAPPABLE);
		createEAttribute(javaTypeEClass, JAVA_TYPE__PRIMITIVE);
		createEReference(javaTypeEClass, JAVA_TYPE__NAMESPACE);

		methodParameterEClass = createEClass(METHOD_PARAMETER);
		createEAttribute(methodParameterEClass, METHOD_PARAMETER__NAME);
		createEReference(methodParameterEClass, METHOD_PARAMETER__METHOD);
		createEReference(methodParameterEClass, METHOD_PARAMETER__TYPE);
		createEAttribute(methodParameterEClass, METHOD_PARAMETER__MODIFIER);
		createEAttribute(methodParameterEClass, METHOD_PARAMETER__HINT);

		namespaceEClass = createEClass(NAMESPACE);
		createEAttribute(namespaceEClass, NAMESPACE__NAME);
		createEReference(namespaceEClass, NAMESPACE__PARENT);
		createEReference(namespaceEClass, NAMESPACE__CHILD_NAMESPACES);
		createEReference(namespaceEClass, NAMESPACE__JAVA_TYPES);
		createEReference(namespaceEClass, NAMESPACE__PROJECT);

		// Create enums
		javaTypeModifierEnumerationEEnum = createEEnum(JAVA_TYPE_MODIFIER_ENUMERATION);
		enumLiteralTagEnumerationEEnum = createEEnum(ENUM_LITERAL_TAG_ENUMERATION);
		enumTagEnumerationEEnum = createEEnum(ENUM_TAG_ENUMERATION);
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
		final var theProjectPackage = (ProjectPackage) EPackage.Registry.INSTANCE.getEPackage(ProjectPackage.eNS_URI);

		// Add supertypes to classes
		javaEnumEClass.getESuperTypes().add(this.getJavaType());

		// Initialize classes and features; add operations and parameters
		initEClass(enumLiteralEClass, EnumLiteral.class, "EnumLiteral", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getEnumLiteral_Name(), ecorePackage.getEString(), "name", null, 0, 1, EnumLiteral.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getEnumLiteral_Tag(), this.getEnumLiteralTagEnumeration(), "tag", null, 0, 1, EnumLiteral.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getEnumLiteral_JavaEnum(), this.getJavaEnum(), this.getJavaEnum_EnumerationValues(), "javaEnum", null, 0, 1,
				EnumLiteral.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE,
				IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(javaEnumEClass, JavaEnum.class, "JavaEnum", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getJavaEnum_EnumerationValues(), this.getEnumLiteral(), this.getEnumLiteral_JavaEnum(), "enumerationValues",
				null, 0, -1, JavaEnum.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES,
				!IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getJavaEnum_Tag(), this.getEnumTagEnumeration(), "tag", null, 0, 1, JavaEnum.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(javaMethodEClass, JavaMethod.class, "JavaMethod", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getJavaMethod_Name(), ecorePackage.getEString(), "name", null, 0, 1, JavaMethod.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getJavaMethod_Comment(), ecorePackage.getEString(), "comment", null, 0, 1, JavaMethod.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getJavaMethod_JavaType(), this.getJavaType(), null, "javaType", null, 0, 1, JavaMethod.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getJavaMethod_ReturnType(), this.getJavaType(), null, "returnType", null, 0, 1, JavaMethod.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getJavaMethod_ReturnTypeModifier(), this.getJavaTypeModifierEnumeration(), "returnTypeModifier", null, 0, 1,
				JavaMethod.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getJavaMethod_MethodParameters(), this.getMethodParameter(), this.getMethodParameter_Method(),
				"methodParameters", null, 0, -1, JavaMethod.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE,
				!IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(javaTypeEClass, JavaType.class, "JavaType", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getJavaType_Name(), ecorePackage.getEString(), "name", null, 0, 1, JavaType.class, !IS_TRANSIENT, !IS_VOLATILE,
				IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getJavaType_Comment(), ecorePackage.getEString(), "comment", null, 0, 1, JavaType.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getJavaType_Mappable(), ecorePackage.getEBoolean(), "mappable", null, 0, 1, JavaType.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getJavaType_Primitive(), ecorePackage.getEBoolean(), "primitive", null, 0, 1, JavaType.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getJavaType_Namespace(), this.getNamespace(), this.getNamespace_JavaTypes(), "namespace", null, 0, 1,
				JavaType.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);

		initEClass(methodParameterEClass, MethodParameter.class, "MethodParameter", !IS_ABSTRACT, !IS_INTERFACE,
				IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getMethodParameter_Name(), ecorePackage.getEString(), "name", null, 0, 1, MethodParameter.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getMethodParameter_Method(), this.getJavaMethod(), this.getJavaMethod_MethodParameters(), "method", null, 0, 1,
				MethodParameter.class, IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE,
				IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getMethodParameter_Type(), this.getJavaType(), null, "type", null, 0, 1, MethodParameter.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getMethodParameter_Modifier(), this.getJavaTypeModifierEnumeration(), "modifier", null, 0, 1,
				MethodParameter.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getMethodParameter_Hint(), ecorePackage.getEString(), "hint", null, 0, 1, MethodParameter.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(namespaceEClass, Namespace.class, "Namespace", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getNamespace_Name(), ecorePackage.getEString(), "name", null, 0, 1, Namespace.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getNamespace_Parent(), this.getNamespace(), null, "parent", null, 0, 1, Namespace.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getNamespace_ChildNamespaces(), this.getNamespace(), null, "childNamespaces", null, 0, -1, Namespace.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEReference(getNamespace_JavaTypes(), this.getJavaType(), this.getJavaType_Namespace(), "javaTypes", null, 0, -1,
				Namespace.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEReference(getNamespace_Project(), theProjectPackage.getProject(), null, "project", null, 0, 1, Namespace.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);

		// Initialize enums and add enum literals
		initEEnum(javaTypeModifierEnumerationEEnum, JavaTypeModifierEnumeration.class, "JavaTypeModifierEnumeration");
		addEEnumLiteral(javaTypeModifierEnumerationEEnum, JavaTypeModifierEnumeration.NONE);
		addEEnumLiteral(javaTypeModifierEnumerationEEnum, JavaTypeModifierEnumeration.COLLECTION);
		addEEnumLiteral(javaTypeModifierEnumerationEEnum, JavaTypeModifierEnumeration.ARRAY_LIST);
		addEEnumLiteral(javaTypeModifierEnumerationEEnum, JavaTypeModifierEnumeration.VECTOR);
		addEEnumLiteral(javaTypeModifierEnumerationEEnum, JavaTypeModifierEnumeration.HASH_MAP);
		addEEnumLiteral(javaTypeModifierEnumerationEEnum, JavaTypeModifierEnumeration.HASH_SET);
		addEEnumLiteral(javaTypeModifierEnumerationEEnum, JavaTypeModifierEnumeration.LIST);

		initEEnum(enumLiteralTagEnumerationEEnum, EnumLiteralTagEnumeration.class, "EnumLiteralTagEnumeration");
		addEEnumLiteral(enumLiteralTagEnumerationEEnum, EnumLiteralTagEnumeration.NONE);
		addEEnumLiteral(enumLiteralTagEnumerationEEnum, EnumLiteralTagEnumeration.LOGGING_LEVEL_WARN);
		addEEnumLiteral(enumLiteralTagEnumerationEEnum, EnumLiteralTagEnumeration.LOGGING_LEVEL_ERROR);
		addEEnumLiteral(enumLiteralTagEnumerationEEnum, EnumLiteralTagEnumeration.LOGGING_LEVEL_INFO);
		addEEnumLiteral(enumLiteralTagEnumerationEEnum, EnumLiteralTagEnumeration.LOGGING_LEVEL_DEBUG);

		initEEnum(enumTagEnumerationEEnum, EnumTagEnumeration.class, "EnumTagEnumeration");
		addEEnumLiteral(enumTagEnumerationEEnum, EnumTagEnumeration.NONE);
		addEEnumLiteral(enumTagEnumerationEEnum, EnumTagEnumeration.LOGGING_LEVEL);

		// Create resource
		createResource(eNS_URI);
	}

}
