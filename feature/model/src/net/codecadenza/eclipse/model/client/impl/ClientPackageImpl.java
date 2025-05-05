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
package net.codecadenza.eclipse.model.client.impl;

import net.codecadenza.eclipse.model.boundary.BoundaryPackage;
import net.codecadenza.eclipse.model.boundary.impl.BoundaryPackageImpl;
import net.codecadenza.eclipse.model.client.ActionType;
import net.codecadenza.eclipse.model.client.ClientFactory;
import net.codecadenza.eclipse.model.client.ClientPackage;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormAction;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormFieldTypeEnumeration;
import net.codecadenza.eclipse.model.client.FormGroup;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.client.FormTable;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.client.TableColumnField;
import net.codecadenza.eclipse.model.client.TableColumnFieldTypeEnumeration;
import net.codecadenza.eclipse.model.client.TreeNode;
import net.codecadenza.eclipse.model.client.TreeSearchItem;
import net.codecadenza.eclipse.model.client.TreeView;
import net.codecadenza.eclipse.model.client.TreeViewItem;
import net.codecadenza.eclipse.model.db.DbPackage;
import net.codecadenza.eclipse.model.db.impl.DbPackageImpl;
import net.codecadenza.eclipse.model.domain.DomainPackage;
import net.codecadenza.eclipse.model.domain.impl.DomainPackageImpl;
import net.codecadenza.eclipse.model.dto.DtoPackage;
import net.codecadenza.eclipse.model.dto.impl.DtoPackageImpl;
import net.codecadenza.eclipse.model.exchange.ExchangePackage;
import net.codecadenza.eclipse.model.exchange.impl.ExchangePackageImpl;
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
 * The <b>Package</b> for the model. It contains accessors for the meta objects to represent
 * <ul>
 * <li>each class,</li>
 * <li>each feature of each class,</li>
 * <li>each enum,</li>
 * <li>and each data type</li>
 * </ul>
 * @generated
 */
public class ClientPackageImpl extends EPackageImpl implements ClientPackage {
	/**
	 * @generated
	 */
	private EClass formEClass;

	/**
	 * @generated
	 */
	private EClass formActionEClass;

	/**
	 * @generated
	 */
	private EClass formFieldEClass;

	/**
	 * @generated
	 */
	private EClass formGroupEClass;

	/**
	 * @generated
	 */
	private EClass formPanelEClass;

	/**
	 * @generated
	 */
	private EClass formTableEClass;

	/**
	 * @generated
	 */
	private EClass tableColumnFieldEClass;

	/**
	 * @generated
	 */
	private EClass treeViewEClass;

	/**
	 * @generated
	 */
	private EClass treeViewItemEClass;

	/**
	 * @generated
	 */
	private EClass treeSearchItemEClass;

	/**
	 * @generated
	 */
	private EClass treeNodeEClass;

	/**
	 * @generated
	 */
	private EEnum formFieldTypeEnumerationEEnum;

	/**
	 * @generated
	 */
	private EEnum formTypeEnumerationEEnum;

	/**
	 * @generated
	 */
	private EEnum tableColumnFieldTypeEnumerationEEnum;

	/**
	 * @generated
	 */
	private EEnum actionTypeEEnum;

	/**
	 * Create an instance of the model <b>Package</b>, registered with {@link org.eclipse.emf.ecore.EPackage.Registry
	 * EPackage.Registry} by the package URI value.
	 * <p>
	 * Note: The correct way to create the package is via the static factory method {@link #init init()}, which also performs
	 * initialization of the package, or returns the registered package, if one already exists.
	 * </p>
	 * @see org.eclipse.emf.ecore.EPackage.Registry
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#eNS_URI
	 * @see #init()
	 * @generated
	 */
	private ClientPackageImpl() {
		super(eNS_URI, ClientFactory.eINSTANCE);
	}

	/**
	 * @generated
	 */
	private static boolean isInited;

	/**
	 * Create, register and initialize the <b>Package</b> for this model, and for any others upon which it depends. This method is
	 * used to initialize {@link ClientPackage#eINSTANCE} when that field is accessed. Clients should not invoke it directly.
	 * Instead, they should simply access that field to obtain the package.
	 * @see #eNS_URI
	 * @see #createPackageContents()
	 * @see #initializePackageContents()
	 * @return the initialized client package
	 * @generated
	 */
	public static ClientPackage init() {
		if (isInited)
			return (ClientPackage) EPackage.Registry.INSTANCE.getEPackage(ClientPackage.eNS_URI);

		// Obtain or create and register package
		final var theClientPackage = (ClientPackageImpl) (EPackage.Registry.INSTANCE.get(eNS_URI) instanceof ClientPackageImpl
				? EPackage.Registry.INSTANCE.get(eNS_URI) : new ClientPackageImpl());

		isInited = true;

		// Obtain or create and register interdependencies
		final var theBoundaryPackage = (BoundaryPackageImpl) (EPackage.Registry.INSTANCE
				.getEPackage(BoundaryPackage.eNS_URI) instanceof BoundaryPackageImpl
						? EPackage.Registry.INSTANCE.getEPackage(BoundaryPackage.eNS_URI) : BoundaryPackage.eINSTANCE);
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
		final var theJavaPackage = (JavaPackageImpl) (EPackage.Registry.INSTANCE
				.getEPackage(JavaPackage.eNS_URI) instanceof JavaPackageImpl ? EPackage.Registry.INSTANCE.getEPackage(JavaPackage.eNS_URI)
						: JavaPackage.eINSTANCE);
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
		theClientPackage.createPackageContents();
		theBoundaryPackage.createPackageContents();
		theRepositoryPackage.createPackageContents();
		theDbPackage.createPackageContents();
		theDomainPackage.createPackageContents();
		theDtoPackage.createPackageContents();
		theJavaPackage.createPackageContents();
		theProjectPackage.createPackageContents();
		theExchangePackage.createPackageContents();
		theServicePackage.createPackageContents();
		theMappingPackage.createPackageContents();

		// Initialize created meta-data
		theClientPackage.initializePackageContents();
		theBoundaryPackage.initializePackageContents();
		theRepositoryPackage.initializePackageContents();
		theDbPackage.initializePackageContents();
		theDomainPackage.initializePackageContents();
		theDtoPackage.initializePackageContents();
		theJavaPackage.initializePackageContents();
		theProjectPackage.initializePackageContents();
		theExchangePackage.initializePackageContents();
		theServicePackage.initializePackageContents();
		theMappingPackage.initializePackageContents();

		// Mark meta-data to indicate it can't be changed
		theClientPackage.freeze();

		// Update the registry and return the package
		EPackage.Registry.INSTANCE.put(ClientPackage.eNS_URI, theClientPackage);
		return theClientPackage;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getForm()
	 * @generated
	 */
	@Override
	public EClass getForm() {
		return formEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getForm_Name()
	 * @generated
	 */
	@Override
	public EAttribute getForm_Name() {
		return (EAttribute) formEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getForm_FormType()
	 * @generated
	 */
	@Override
	public EAttribute getForm_FormType() {
		return (EAttribute) formEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getForm_FormGroup()
	 * @generated
	 */
	@Override
	public EReference getForm_FormGroup() {
		return (EReference) formEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getForm_Title()
	 * @generated
	 */
	@Override
	public EAttribute getForm_Title() {
		return (EAttribute) formEClass.getEStructuralFeatures().get(3);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getForm_Modal()
	 * @generated
	 */
	@Override
	public EAttribute getForm_Modal() {
		return (EAttribute) formEClass.getEStructuralFeatures().get(4);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getForm_Resizable()
	 * @generated
	 */
	@Override
	public EAttribute getForm_Resizable() {
		return (EAttribute) formEClass.getEStructuralFeatures().get(5);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getForm_TitleArea()
	 * @generated
	 */
	@Override
	public EAttribute getForm_TitleArea() {
		return (EAttribute) formEClass.getEStructuralFeatures().get(6);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getForm_OpenEditAfterCreate()
	 * @generated
	 */
	@Override
	public EAttribute getForm_OpenEditAfterCreate() {
		return (EAttribute) formEClass.getEStructuralFeatures().get(7);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getForm_Height()
	 * @generated
	 */
	@Override
	public EAttribute getForm_Height() {
		return (EAttribute) formEClass.getEStructuralFeatures().get(8);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getForm_Width()
	 * @generated
	 */
	@Override
	public EAttribute getForm_Width() {
		return (EAttribute) formEClass.getEStructuralFeatures().get(9);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getForm_FormPanels()
	 * @generated
	 */
	@Override
	public EReference getForm_FormPanels() {
		return (EReference) formEClass.getEStructuralFeatures().get(10);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getForm_Actions()
	 * @generated
	 */
	@Override
	public EReference getForm_Actions() {
		return (EReference) formEClass.getEStructuralFeatures().get(11);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getForm_DTO()
	 * @generated
	 */
	@Override
	public EReference getForm_DTO() {
		return (EReference) formEClass.getEStructuralFeatures().get(12);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getForm_DomainObject()
	 * @generated
	 */
	@Override
	public EReference getForm_DomainObject() {
		return (EReference) formEClass.getEStructuralFeatures().get(13);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getForm_Roles()
	 * @generated
	 */
	@Override
	public EReference getForm_Roles() {
		return (EReference) formEClass.getEStructuralFeatures().get(14);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getForm_BoundaryMethod()
	 * @generated
	 */
	@Override
	public EReference getForm_BoundaryMethod() {
		return (EReference) formEClass.getEStructuralFeatures().get(15);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormAction()
	 * @generated
	 */
	@Override
	public EClass getFormAction() {
		return formActionEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormAction_Name()
	 * @generated
	 */
	@Override
	public EAttribute getFormAction_Name() {
		return (EAttribute) formActionEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormAction_Description()
	 * @generated
	 */
	@Override
	public EAttribute getFormAction_Description() {
		return (EAttribute) formActionEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormAction_Form()
	 * @generated
	 */
	@Override
	public EReference getFormAction_Form() {
		return (EReference) formActionEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormAction_Panel()
	 * @generated
	 */
	@Override
	public EReference getFormAction_Panel() {
		return (EReference) formActionEClass.getEStructuralFeatures().get(3);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormAction_TargetForm()
	 * @generated
	 */
	@Override
	public EReference getFormAction_TargetForm() {
		return (EReference) formActionEClass.getEStructuralFeatures().get(4);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormAction_BoundaryMethod()
	 * @generated
	 */
	@Override
	public EReference getFormAction_BoundaryMethod() {
		return (EReference) formActionEClass.getEStructuralFeatures().get(5);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormAction_Roles()
	 * @generated
	 */
	@Override
	public EReference getFormAction_Roles() {
		return (EReference) formActionEClass.getEStructuralFeatures().get(6);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormAction_Type()
	 * @generated
	 */
	@Override
	public EAttribute getFormAction_Type() {
		return (EAttribute) formActionEClass.getEStructuralFeatures().get(7);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormAction_Label()
	 * @generated
	 */
	@Override
	public EAttribute getFormAction_Label() {
		return (EAttribute) formActionEClass.getEStructuralFeatures().get(8);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormField()
	 * @generated
	 */
	@Override
	public EClass getFormField() {
		return formFieldEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormField_Name()
	 * @generated
	 */
	@Override
	public EAttribute getFormField_Name() {
		return (EAttribute) formFieldEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormField_ColIndex()
	 * @generated
	 */
	@Override
	public EAttribute getFormField_ColIndex() {
		return (EAttribute) formFieldEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormField_RowIndex()
	 * @generated
	 */
	@Override
	public EAttribute getFormField_RowIndex() {
		return (EAttribute) formFieldEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormField_Visible()
	 * @generated
	 */
	@Override
	public EAttribute getFormField_Visible() {
		return (EAttribute) formFieldEClass.getEStructuralFeatures().get(3);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormField_Label()
	 * @generated
	 */
	@Override
	public EAttribute getFormField_Label() {
		return (EAttribute) formFieldEClass.getEStructuralFeatures().get(4);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormField_SpanCols()
	 * @generated
	 */
	@Override
	public EAttribute getFormField_SpanCols() {
		return (EAttribute) formFieldEClass.getEStructuralFeatures().get(5);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormField_Readonly()
	 * @generated
	 */
	@Override
	public EAttribute getFormField_Readonly() {
		return (EAttribute) formFieldEClass.getEStructuralFeatures().get(6);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormField_Mandatory()
	 * @generated
	 */
	@Override
	public EAttribute getFormField_Mandatory() {
		return (EAttribute) formFieldEClass.getEStructuralFeatures().get(7);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormField_FieldType()
	 * @generated
	 */
	@Override
	public EAttribute getFormField_FieldType() {
		return (EAttribute) formFieldEClass.getEStructuralFeatures().get(8);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormField_Panel()
	 * @generated
	 */
	@Override
	public EReference getFormField_Panel() {
		return (EReference) formFieldEClass.getEStructuralFeatures().get(9);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormField_Width()
	 * @generated
	 */
	@Override
	public EAttribute getFormField_Width() {
		return (EAttribute) formFieldEClass.getEStructuralFeatures().get(10);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormField_DefaultValue()
	 * @generated
	 */
	@Override
	public EAttribute getFormField_DefaultValue() {
		return (EAttribute) formFieldEClass.getEStructuralFeatures().get(11);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormField_ListOfValues()
	 * @generated
	 */
	@Override
	public EReference getFormField_ListOfValues() {
		return (EReference) formFieldEClass.getEStructuralFeatures().get(12);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormField_DTOAttribute()
	 * @generated
	 */
	@Override
	public EReference getFormField_DTOAttribute() {
		return (EReference) formFieldEClass.getEStructuralFeatures().get(13);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormField_AddFormLinkToLabel()
	 * @generated
	 */
	@Override
	public EAttribute getFormField_AddFormLinkToLabel() {
		return (EAttribute) formFieldEClass.getEStructuralFeatures().get(14);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormGroup()
	 * @generated
	 */
	@Override
	public EClass getFormGroup() {
		return formGroupEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormGroup_Name()
	 * @generated
	 */
	@Override
	public EAttribute getFormGroup_Name() {
		return (EAttribute) formGroupEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormGroup_GroupOrder()
	 * @generated
	 */
	@Override
	public EAttribute getFormGroup_GroupOrder() {
		return (EAttribute) formGroupEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormGroup_ParentGroup()
	 * @generated
	 */
	@Override
	public EReference getFormGroup_ParentGroup() {
		return (EReference) formGroupEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormGroup_ChildGroups()
	 * @generated
	 */
	@Override
	public EReference getFormGroup_ChildGroups() {
		return (EReference) formGroupEClass.getEStructuralFeatures().get(3);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormGroup_Forms()
	 * @generated
	 */
	@Override
	public EReference getFormGroup_Forms() {
		return (EReference) formGroupEClass.getEStructuralFeatures().get(4);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormGroup_Panels()
	 * @generated
	 */
	@Override
	public EReference getFormGroup_Panels() {
		return (EReference) formGroupEClass.getEStructuralFeatures().get(5);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormGroup_Roles()
	 * @generated
	 */
	@Override
	public EReference getFormGroup_Roles() {
		return (EReference) formGroupEClass.getEStructuralFeatures().get(6);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormGroup_Project()
	 * @generated
	 */
	@Override
	public EReference getFormGroup_Project() {
		return (EReference) formGroupEClass.getEStructuralFeatures().get(7);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormPanel()
	 * @generated
	 */
	@Override
	public EClass getFormPanel() {
		return formPanelEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormPanel_Name()
	 * @generated
	 */
	@Override
	public EAttribute getFormPanel_Name() {
		return (EAttribute) formPanelEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormPanel_ColumnCount()
	 * @generated
	 */
	@Override
	public EAttribute getFormPanel_ColumnCount() {
		return (EAttribute) formPanelEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormPanel_ColIndex()
	 * @generated
	 */
	@Override
	public EAttribute getFormPanel_ColIndex() {
		return (EAttribute) formPanelEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormPanel_RowIndex()
	 * @generated
	 */
	@Override
	public EAttribute getFormPanel_RowIndex() {
		return (EAttribute) formPanelEClass.getEStructuralFeatures().get(3);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormPanel_Verticalspan()
	 * @generated
	 */
	@Override
	public EAttribute getFormPanel_Verticalspan() {
		return (EAttribute) formPanelEClass.getEStructuralFeatures().get(4);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormPanel_HorizontalSpan()
	 * @generated
	 */
	@Override
	public EAttribute getFormPanel_HorizontalSpan() {
		return (EAttribute) formPanelEClass.getEStructuralFeatures().get(5);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormPanel_FormGroup()
	 * @generated
	 */
	@Override
	public EReference getFormPanel_FormGroup() {
		return (EReference) formPanelEClass.getEStructuralFeatures().get(6);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormPanel_Label()
	 * @generated
	 */
	@Override
	public EAttribute getFormPanel_Label() {
		return (EAttribute) formPanelEClass.getEStructuralFeatures().get(7);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormPanel_Form()
	 * @generated
	 */
	@Override
	public EReference getFormPanel_Form() {
		return (EReference) formPanelEClass.getEStructuralFeatures().get(8);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormPanel_DrawBorder()
	 * @generated
	 */
	@Override
	public EAttribute getFormPanel_DrawBorder() {
		return (EAttribute) formPanelEClass.getEStructuralFeatures().get(9);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormPanel_BasePanel()
	 * @generated
	 */
	@Override
	public EReference getFormPanel_BasePanel() {
		return (EReference) formPanelEClass.getEStructuralFeatures().get(10);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormPanel_FormTable()
	 * @generated
	 */
	@Override
	public EReference getFormPanel_FormTable() {
		return (EReference) formPanelEClass.getEStructuralFeatures().get(11);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormPanel_Fields()
	 * @generated
	 */
	@Override
	public EReference getFormPanel_Fields() {
		return (EReference) formPanelEClass.getEStructuralFeatures().get(12);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormPanel_Actions()
	 * @generated
	 */
	@Override
	public EReference getFormPanel_Actions() {
		return (EReference) formPanelEClass.getEStructuralFeatures().get(13);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormPanel_DTO()
	 * @generated
	 */
	@Override
	public EReference getFormPanel_DTO() {
		return (EReference) formPanelEClass.getEStructuralFeatures().get(14);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormPanel_BoundaryMethod()
	 * @generated
	 */
	@Override
	public EReference getFormPanel_BoundaryMethod() {
		return (EReference) formPanelEClass.getEStructuralFeatures().get(15);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormPanel_Association()
	 * @generated
	 */
	@Override
	public EReference getFormPanel_Association() {
		return (EReference) formPanelEClass.getEStructuralFeatures().get(16);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormTable()
	 * @generated
	 */
	@Override
	public EClass getFormTable() {
		return formTableEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormTable_Name()
	 * @generated
	 */
	@Override
	public EAttribute getFormTable_Name() {
		return (EAttribute) formTableEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormTable_ColIndex()
	 * @generated
	 */
	@Override
	public EAttribute getFormTable_ColIndex() {
		return (EAttribute) formTableEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormTable_RowIndex()
	 * @generated
	 */
	@Override
	public EAttribute getFormTable_RowIndex() {
		return (EAttribute) formTableEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormTable_SpanCols()
	 * @generated
	 */
	@Override
	public EAttribute getFormTable_SpanCols() {
		return (EAttribute) formTableEClass.getEStructuralFeatures().get(3);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormTable_SpanRows()
	 * @generated
	 */
	@Override
	public EAttribute getFormTable_SpanRows() {
		return (EAttribute) formTableEClass.getEStructuralFeatures().get(4);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormTable_VerticalSpan()
	 * @generated
	 */
	@Override
	public EAttribute getFormTable_VerticalSpan() {
		return (EAttribute) formTableEClass.getEStructuralFeatures().get(5);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormTable_HorizontalSpan()
	 * @generated
	 */
	@Override
	public EAttribute getFormTable_HorizontalSpan() {
		return (EAttribute) formTableEClass.getEStructuralFeatures().get(6);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormTable_FormPanel()
	 * @generated
	 */
	@Override
	public EReference getFormTable_FormPanel() {
		return (EReference) formTableEClass.getEStructuralFeatures().get(7);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormTable_Fields()
	 * @generated
	 */
	@Override
	public EReference getFormTable_Fields() {
		return (EReference) formTableEClass.getEStructuralFeatures().get(8);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormTable_Association()
	 * @generated
	 */
	@Override
	public EReference getFormTable_Association() {
		return (EReference) formTableEClass.getEStructuralFeatures().get(9);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTableColumnField()
	 * @generated
	 */
	@Override
	public EClass getTableColumnField() {
		return tableColumnFieldEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTableColumnField_ColIndex()
	 * @generated
	 */
	@Override
	public EAttribute getTableColumnField_ColIndex() {
		return (EAttribute) tableColumnFieldEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTableColumnField_Visible()
	 * @generated
	 */
	@Override
	public EAttribute getTableColumnField_Visible() {
		return (EAttribute) tableColumnFieldEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTableColumnField_Identifier()
	 * @generated
	 */
	@Override
	public EAttribute getTableColumnField_Identifier() {
		return (EAttribute) tableColumnFieldEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTableColumnField_FormTable()
	 * @generated
	 */
	@Override
	public EReference getTableColumnField_FormTable() {
		return (EReference) tableColumnFieldEClass.getEStructuralFeatures().get(3);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTableColumnField_Width()
	 * @generated
	 */
	@Override
	public EAttribute getTableColumnField_Width() {
		return (EAttribute) tableColumnFieldEClass.getEStructuralFeatures().get(4);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTableColumnField_Title()
	 * @generated
	 */
	@Override
	public EAttribute getTableColumnField_Title() {
		return (EAttribute) tableColumnFieldEClass.getEStructuralFeatures().get(5);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTableColumnField_LovForm()
	 * @generated
	 */
	@Override
	public EReference getTableColumnField_LovForm() {
		return (EReference) tableColumnFieldEClass.getEStructuralFeatures().get(6);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTableColumnField_FieldType()
	 * @generated
	 */
	@Override
	public EAttribute getTableColumnField_FieldType() {
		return (EAttribute) tableColumnFieldEClass.getEStructuralFeatures().get(7);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTableColumnField_AssociationRef()
	 * @generated
	 */
	@Override
	public EAttribute getTableColumnField_AssociationRef() {
		return (EAttribute) tableColumnFieldEClass.getEStructuralFeatures().get(8);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTableColumnField_Searchable()
	 * @generated
	 */
	@Override
	public EAttribute getTableColumnField_Searchable() {
		return (EAttribute) tableColumnFieldEClass.getEStructuralFeatures().get(9);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTableColumnField_DTOAttribute()
	 * @generated
	 */
	@Override
	public EReference getTableColumnField_DTOAttribute() {
		return (EReference) tableColumnFieldEClass.getEStructuralFeatures().get(10);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTreeView()
	 * @generated
	 */
	@Override
	public EClass getTreeView() {
		return treeViewEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTreeView_RootTreeItem()
	 * @generated
	 */
	@Override
	public EReference getTreeView_RootTreeItem() {
		return (EReference) treeViewEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTreeView_QuickSearchItems()
	 * @generated
	 */
	@Override
	public EReference getTreeView_QuickSearchItems() {
		return (EReference) treeViewEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTreeView_AdvancedSearchItems()
	 * @generated
	 */
	@Override
	public EReference getTreeView_AdvancedSearchItems() {
		return (EReference) treeViewEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTreeView_CountMethod()
	 * @generated
	 */
	@Override
	public EReference getTreeView_CountMethod() {
		return (EReference) treeViewEClass.getEStructuralFeatures().get(3);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTreeView_RecursiveMethod()
	 * @generated
	 */
	@Override
	public EReference getTreeView_RecursiveMethod() {
		return (EReference) treeViewEClass.getEStructuralFeatures().get(4);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTreeViewItem()
	 * @generated
	 */
	@Override
	public EClass getTreeViewItem() {
		return treeViewItemEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTreeViewItem_ParentItem()
	 * @generated
	 */
	@Override
	public EReference getTreeViewItem_ParentItem() {
		return (EReference) treeViewItemEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTreeViewItem_Children()
	 * @generated
	 */
	@Override
	public EReference getTreeViewItem_Children() {
		return (EReference) treeViewItemEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTreeViewItem_DataFetchMethod()
	 * @generated
	 */
	@Override
	public EReference getTreeViewItem_DataFetchMethod() {
		return (EReference) treeViewItemEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTreeViewItem_DropMethod()
	 * @generated
	 */
	@Override
	public EReference getTreeViewItem_DropMethod() {
		return (EReference) treeViewItemEClass.getEStructuralFeatures().get(3);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTreeViewItem_DisplayAttributes()
	 * @generated
	 */
	@Override
	public EReference getTreeViewItem_DisplayAttributes() {
		return (EReference) treeViewItemEClass.getEStructuralFeatures().get(4);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTreeViewItem_Association()
	 * @generated
	 */
	@Override
	public EReference getTreeViewItem_Association() {
		return (EReference) treeViewItemEClass.getEStructuralFeatures().get(5);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTreeViewItem_Nodes()
	 * @generated
	 */
	@Override
	public EReference getTreeViewItem_Nodes() {
		return (EReference) treeViewItemEClass.getEStructuralFeatures().get(6);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTreeViewItem_Label()
	 * @generated
	 */
	@Override
	public EAttribute getTreeViewItem_Label() {
		return (EAttribute) treeViewItemEClass.getEStructuralFeatures().get(7);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTreeViewItem_ItemDTO()
	 * @generated
	 */
	@Override
	public EReference getTreeViewItem_ItemDTO() {
		return (EReference) treeViewItemEClass.getEStructuralFeatures().get(8);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTreeViewItem_InvisibleAttributes()
	 * @generated
	 */
	@Override
	public EReference getTreeViewItem_InvisibleAttributes() {
		return (EReference) treeViewItemEClass.getEStructuralFeatures().get(9);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTreeSearchItem()
	 * @generated
	 */
	@Override
	public EClass getTreeSearchItem() {
		return treeSearchItemEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTreeSearchItem_Label()
	 * @generated
	 */
	@Override
	public EAttribute getTreeSearchItem_Label() {
		return (EAttribute) treeSearchItemEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTreeSearchItem_DTOAttribute()
	 * @generated
	 */
	@Override
	public EReference getTreeSearchItem_DTOAttribute() {
		return (EReference) treeSearchItemEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTreeNode()
	 * @generated
	 */
	@Override
	public EClass getTreeNode() {
		return treeNodeEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTreeNode_Label()
	 * @generated
	 */
	@Override
	public EAttribute getTreeNode_Label() {
		return (EAttribute) treeNodeEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTreeNode_DTOAttribute()
	 * @generated
	 */
	@Override
	public EReference getTreeNode_DTOAttribute() {
		return (EReference) treeNodeEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormFieldTypeEnumeration()
	 * @generated
	 */
	@Override
	public EEnum getFormFieldTypeEnumeration() {
		return formFieldTypeEnumerationEEnum;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getFormTypeEnumeration()
	 * @generated
	 */
	@Override
	public EEnum getFormTypeEnumeration() {
		return formTypeEnumerationEEnum;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getTableColumnFieldTypeEnumeration()
	 * @generated
	 */
	@Override
	public EEnum getTableColumnFieldTypeEnumeration() {
		return tableColumnFieldTypeEnumerationEEnum;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getActionType()
	 * @generated
	 */
	@Override
	public EEnum getActionType() {
		return actionTypeEEnum;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.client.ClientPackage#getClientFactory()
	 * @generated
	 */
	@Override
	public ClientFactory getClientFactory() {
		return (ClientFactory) getEFactoryInstance();
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
		formEClass = createEClass(FORM);
		createEAttribute(formEClass, FORM__NAME);
		createEAttribute(formEClass, FORM__FORM_TYPE);
		createEReference(formEClass, FORM__FORM_GROUP);
		createEAttribute(formEClass, FORM__TITLE);
		createEAttribute(formEClass, FORM__MODAL);
		createEAttribute(formEClass, FORM__RESIZABLE);
		createEAttribute(formEClass, FORM__TITLE_AREA);
		createEAttribute(formEClass, FORM__OPEN_EDIT_AFTER_CREATE);
		createEAttribute(formEClass, FORM__HEIGHT);
		createEAttribute(formEClass, FORM__WIDTH);
		createEReference(formEClass, FORM__FORM_PANELS);
		createEReference(formEClass, FORM__ACTIONS);
		createEReference(formEClass, FORM__DTO);
		createEReference(formEClass, FORM__DOMAIN_OBJECT);
		createEReference(formEClass, FORM__ROLES);
		createEReference(formEClass, FORM__BOUNDARY_METHOD);

		formActionEClass = createEClass(FORM_ACTION);
		createEAttribute(formActionEClass, FORM_ACTION__NAME);
		createEAttribute(formActionEClass, FORM_ACTION__DESCRIPTION);
		createEReference(formActionEClass, FORM_ACTION__FORM);
		createEReference(formActionEClass, FORM_ACTION__PANEL);
		createEReference(formActionEClass, FORM_ACTION__TARGET_FORM);
		createEReference(formActionEClass, FORM_ACTION__BOUNDARY_METHOD);
		createEReference(formActionEClass, FORM_ACTION__ROLES);
		createEAttribute(formActionEClass, FORM_ACTION__TYPE);
		createEAttribute(formActionEClass, FORM_ACTION__LABEL);

		formFieldEClass = createEClass(FORM_FIELD);
		createEAttribute(formFieldEClass, FORM_FIELD__NAME);
		createEAttribute(formFieldEClass, FORM_FIELD__COL_INDEX);
		createEAttribute(formFieldEClass, FORM_FIELD__ROW_INDEX);
		createEAttribute(formFieldEClass, FORM_FIELD__VISIBLE);
		createEAttribute(formFieldEClass, FORM_FIELD__LABEL);
		createEAttribute(formFieldEClass, FORM_FIELD__SPAN_COLS);
		createEAttribute(formFieldEClass, FORM_FIELD__READONLY);
		createEAttribute(formFieldEClass, FORM_FIELD__MANDATORY);
		createEAttribute(formFieldEClass, FORM_FIELD__FIELD_TYPE);
		createEReference(formFieldEClass, FORM_FIELD__PANEL);
		createEAttribute(formFieldEClass, FORM_FIELD__WIDTH);
		createEAttribute(formFieldEClass, FORM_FIELD__DEFAULT_VALUE);
		createEReference(formFieldEClass, FORM_FIELD__LIST_OF_VALUES);
		createEReference(formFieldEClass, FORM_FIELD__DTO_ATTRIBUTE);
		createEAttribute(formFieldEClass, FORM_FIELD__ADD_FORM_LINK_TO_LABEL);

		formGroupEClass = createEClass(FORM_GROUP);
		createEAttribute(formGroupEClass, FORM_GROUP__NAME);
		createEAttribute(formGroupEClass, FORM_GROUP__GROUP_ORDER);
		createEReference(formGroupEClass, FORM_GROUP__PARENT_GROUP);
		createEReference(formGroupEClass, FORM_GROUP__CHILD_GROUPS);
		createEReference(formGroupEClass, FORM_GROUP__FORMS);
		createEReference(formGroupEClass, FORM_GROUP__PANELS);
		createEReference(formGroupEClass, FORM_GROUP__ROLES);
		createEReference(formGroupEClass, FORM_GROUP__PROJECT);

		formPanelEClass = createEClass(FORM_PANEL);
		createEAttribute(formPanelEClass, FORM_PANEL__NAME);
		createEAttribute(formPanelEClass, FORM_PANEL__COLUMN_COUNT);
		createEAttribute(formPanelEClass, FORM_PANEL__COL_INDEX);
		createEAttribute(formPanelEClass, FORM_PANEL__ROW_INDEX);
		createEAttribute(formPanelEClass, FORM_PANEL__VERTICALSPAN);
		createEAttribute(formPanelEClass, FORM_PANEL__HORIZONTAL_SPAN);
		createEReference(formPanelEClass, FORM_PANEL__FORM_GROUP);
		createEAttribute(formPanelEClass, FORM_PANEL__LABEL);
		createEReference(formPanelEClass, FORM_PANEL__FORM);
		createEAttribute(formPanelEClass, FORM_PANEL__DRAW_BORDER);
		createEReference(formPanelEClass, FORM_PANEL__BASE_PANEL);
		createEReference(formPanelEClass, FORM_PANEL__FORM_TABLE);
		createEReference(formPanelEClass, FORM_PANEL__FIELDS);
		createEReference(formPanelEClass, FORM_PANEL__ACTIONS);
		createEReference(formPanelEClass, FORM_PANEL__DTO);
		createEReference(formPanelEClass, FORM_PANEL__BOUNDARY_METHOD);
		createEReference(formPanelEClass, FORM_PANEL__ASSOCIATION);

		formTableEClass = createEClass(FORM_TABLE);
		createEAttribute(formTableEClass, FORM_TABLE__NAME);
		createEAttribute(formTableEClass, FORM_TABLE__COL_INDEX);
		createEAttribute(formTableEClass, FORM_TABLE__ROW_INDEX);
		createEAttribute(formTableEClass, FORM_TABLE__SPAN_COLS);
		createEAttribute(formTableEClass, FORM_TABLE__SPAN_ROWS);
		createEAttribute(formTableEClass, FORM_TABLE__VERTICAL_SPAN);
		createEAttribute(formTableEClass, FORM_TABLE__HORIZONTAL_SPAN);
		createEReference(formTableEClass, FORM_TABLE__FORM_PANEL);
		createEReference(formTableEClass, FORM_TABLE__FIELDS);
		createEReference(formTableEClass, FORM_TABLE__ASSOCIATION);

		tableColumnFieldEClass = createEClass(TABLE_COLUMN_FIELD);
		createEAttribute(tableColumnFieldEClass, TABLE_COLUMN_FIELD__COL_INDEX);
		createEAttribute(tableColumnFieldEClass, TABLE_COLUMN_FIELD__VISIBLE);
		createEAttribute(tableColumnFieldEClass, TABLE_COLUMN_FIELD__IDENTIFIER);
		createEReference(tableColumnFieldEClass, TABLE_COLUMN_FIELD__FORM_TABLE);
		createEAttribute(tableColumnFieldEClass, TABLE_COLUMN_FIELD__WIDTH);
		createEAttribute(tableColumnFieldEClass, TABLE_COLUMN_FIELD__TITLE);
		createEReference(tableColumnFieldEClass, TABLE_COLUMN_FIELD__LOV_FORM);
		createEAttribute(tableColumnFieldEClass, TABLE_COLUMN_FIELD__FIELD_TYPE);
		createEAttribute(tableColumnFieldEClass, TABLE_COLUMN_FIELD__ASSOCIATION_REF);
		createEAttribute(tableColumnFieldEClass, TABLE_COLUMN_FIELD__SEARCHABLE);
		createEReference(tableColumnFieldEClass, TABLE_COLUMN_FIELD__DTO_ATTRIBUTE);

		treeViewEClass = createEClass(TREE_VIEW);
		createEReference(treeViewEClass, TREE_VIEW__ROOT_TREE_ITEM);
		createEReference(treeViewEClass, TREE_VIEW__QUICK_SEARCH_ITEMS);
		createEReference(treeViewEClass, TREE_VIEW__ADVANCED_SEARCH_ITEMS);
		createEReference(treeViewEClass, TREE_VIEW__COUNT_METHOD);
		createEReference(treeViewEClass, TREE_VIEW__RECURSIVE_METHOD);

		treeViewItemEClass = createEClass(TREE_VIEW_ITEM);
		createEReference(treeViewItemEClass, TREE_VIEW_ITEM__PARENT_ITEM);
		createEReference(treeViewItemEClass, TREE_VIEW_ITEM__CHILDREN);
		createEReference(treeViewItemEClass, TREE_VIEW_ITEM__DATA_FETCH_METHOD);
		createEReference(treeViewItemEClass, TREE_VIEW_ITEM__DROP_METHOD);
		createEReference(treeViewItemEClass, TREE_VIEW_ITEM__DISPLAY_ATTRIBUTES);
		createEReference(treeViewItemEClass, TREE_VIEW_ITEM__ASSOCIATION);
		createEReference(treeViewItemEClass, TREE_VIEW_ITEM__NODES);
		createEAttribute(treeViewItemEClass, TREE_VIEW_ITEM__LABEL);
		createEReference(treeViewItemEClass, TREE_VIEW_ITEM__ITEM_DTO);
		createEReference(treeViewItemEClass, TREE_VIEW_ITEM__INVISIBLE_ATTRIBUTES);

		treeSearchItemEClass = createEClass(TREE_SEARCH_ITEM);
		createEAttribute(treeSearchItemEClass, TREE_SEARCH_ITEM__LABEL);
		createEReference(treeSearchItemEClass, TREE_SEARCH_ITEM__DTO_ATTRIBUTE);

		treeNodeEClass = createEClass(TREE_NODE);
		createEAttribute(treeNodeEClass, TREE_NODE__LABEL);
		createEReference(treeNodeEClass, TREE_NODE__DTO_ATTRIBUTE);

		// Create enums
		formFieldTypeEnumerationEEnum = createEEnum(FORM_FIELD_TYPE_ENUMERATION);
		formTypeEnumerationEEnum = createEEnum(FORM_TYPE_ENUMERATION);
		tableColumnFieldTypeEnumerationEEnum = createEEnum(TABLE_COLUMN_FIELD_TYPE_ENUMERATION);
		actionTypeEEnum = createEEnum(ACTION_TYPE);
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
		final var theDtoPackage = (DtoPackage) EPackage.Registry.INSTANCE.getEPackage(DtoPackage.eNS_URI);
		final var theDomainPackage = (DomainPackage) EPackage.Registry.INSTANCE.getEPackage(DomainPackage.eNS_URI);
		final var theProjectPackage = (ProjectPackage) EPackage.Registry.INSTANCE.getEPackage(ProjectPackage.eNS_URI);
		final var theBoundaryPackage = (BoundaryPackage) EPackage.Registry.INSTANCE.getEPackage(BoundaryPackage.eNS_URI);

		// Add supertypes to classes
		treeViewEClass.getESuperTypes().add(this.getForm());

		// Initialize classes and features; add operations and parameters
		initEClass(formEClass, Form.class, "Form", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getForm_Name(), ecorePackage.getEString(), "name", null, 0, 1, Form.class, !IS_TRANSIENT, !IS_VOLATILE,
				IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getForm_FormType(), this.getFormTypeEnumeration(), "formType", null, 0, 1, Form.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getForm_FormGroup(), this.getFormGroup(), this.getFormGroup_Forms(), "formGroup", null, 0, 1, Form.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getForm_Title(), ecorePackage.getEString(), "title", null, 0, 1, Form.class, !IS_TRANSIENT, !IS_VOLATILE,
				IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getForm_Modal(), ecorePackage.getEBoolean(), "modal", "false", 0, 1, Form.class, !IS_TRANSIENT, !IS_VOLATILE,
				IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getForm_Resizable(), ecorePackage.getEBoolean(), "resizable", "true", 0, 1, Form.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getForm_TitleArea(), ecorePackage.getEBoolean(), "titleArea", "true", 0, 1, Form.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getForm_OpenEditAfterCreate(), ecorePackage.getEBoolean(), "openEditAfterCreate", "false", 0, 1, Form.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getForm_Height(), ecorePackage.getEInt(), "height", null, 0, 1, Form.class, !IS_TRANSIENT, !IS_VOLATILE,
				IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getForm_Width(), ecorePackage.getEInt(), "width", null, 0, 1, Form.class, !IS_TRANSIENT, !IS_VOLATILE,
				IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getForm_FormPanels(), this.getFormPanel(), this.getFormPanel_Form(), "formPanels", null, 0, -1, Form.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEReference(getForm_Actions(), this.getFormAction(), this.getFormAction_Form(), "actions", null, 0, -1, Form.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEReference(getForm_DTO(), theDtoPackage.getDTOBean(), null, "dTO", null, 0, 1, Form.class, !IS_TRANSIENT, !IS_VOLATILE,
				IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getForm_DomainObject(), theDomainPackage.getDomainObject(), null, "domainObject", null, 0, 1, Form.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEReference(getForm_Roles(), theProjectPackage.getRole(), null, "roles", null, 0, -1, Form.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getForm_BoundaryMethod(), theBoundaryPackage.getBoundaryMethod(), null, "boundaryMethod", null, 0, 1,
				Form.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);

		initEClass(formActionEClass, FormAction.class, "FormAction", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getFormAction_Name(), ecorePackage.getEString(), "name", null, 0, 1, FormAction.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getFormAction_Description(), ecorePackage.getEString(), "description", null, 0, 1, FormAction.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getFormAction_Form(), this.getForm(), this.getForm_Actions(), "form", null, 0, 1, FormAction.class,
				IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEReference(getFormAction_Panel(), this.getFormPanel(), this.getFormPanel_Actions(), "panel", null, 0, 1, FormAction.class,
				IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEReference(getFormAction_TargetForm(), this.getForm(), null, "targetForm", null, 0, 1, FormAction.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getFormAction_BoundaryMethod(), theBoundaryPackage.getBoundaryMethod(), null, "boundaryMethod", null, 0, 1,
				FormAction.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE,
				IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getFormAction_Roles(), theProjectPackage.getRole(), null, "roles", null, 0, -1, FormAction.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getFormAction_Type(), this.getActionType(), "type", null, 0, 1, FormAction.class, !IS_TRANSIENT, !IS_VOLATILE,
				IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getFormAction_Label(), ecorePackage.getEString(), "label", null, 0, 1, FormAction.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(formFieldEClass, FormField.class, "FormField", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getFormField_Name(), ecorePackage.getEString(), "name", null, 0, 1, FormField.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getFormField_ColIndex(), ecorePackage.getEInt(), "colIndex", "1", 0, 1, FormField.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getFormField_RowIndex(), ecorePackage.getEInt(), "rowIndex", "1", 0, 1, FormField.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getFormField_Visible(), ecorePackage.getEBoolean(), "visible", "true", 0, 1, FormField.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getFormField_Label(), ecorePackage.getEString(), "label", null, 0, 1, FormField.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getFormField_SpanCols(), ecorePackage.getEBoolean(), "spanCols", "false", 0, 1, FormField.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getFormField_Readonly(), ecorePackage.getEBoolean(), "readonly", "false", 0, 1, FormField.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getFormField_Mandatory(), ecorePackage.getEBoolean(), "mandatory", "true", 0, 1, FormField.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getFormField_FieldType(), this.getFormFieldTypeEnumeration(), "fieldType", null, 0, 1, FormField.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getFormField_Panel(), this.getFormPanel(), this.getFormPanel_Fields(), "panel", null, 0, 1, FormField.class,
				IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getFormField_Width(), ecorePackage.getEInt(), "width", "0", 0, 1, FormField.class, !IS_TRANSIENT, !IS_VOLATILE,
				IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getFormField_DefaultValue(), ecorePackage.getEString(), "defaultValue", null, 0, 1, FormField.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getFormField_ListOfValues(), this.getForm(), null, "listOfValues", null, 0, 1, FormField.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getFormField_DTOAttribute(), theDtoPackage.getDTOBeanAttribute(), null, "dTOAttribute", null, 0, 1,
				FormField.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEAttribute(getFormField_AddFormLinkToLabel(), ecorePackage.getEBoolean(), "addFormLinkToLabel", null, 0, 1,
				FormField.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(formGroupEClass, FormGroup.class, "FormGroup", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getFormGroup_Name(), ecorePackage.getEString(), "name", null, 0, 1, FormGroup.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getFormGroup_GroupOrder(), ecorePackage.getEInt(), "groupOrder", null, 0, 1, FormGroup.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getFormGroup_ParentGroup(), this.getFormGroup(), null, "parentGroup", null, 0, 1, FormGroup.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEReference(getFormGroup_ChildGroups(), this.getFormGroup(), null, "childGroups", null, 0, -1, FormGroup.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEReference(getFormGroup_Forms(), this.getForm(), this.getForm_FormGroup(), "forms", null, 0, -1, FormGroup.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEReference(getFormGroup_Panels(), this.getFormPanel(), this.getFormPanel_FormGroup(), "panels", null, 0, -1,
				FormGroup.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEReference(getFormGroup_Roles(), theProjectPackage.getRole(), null, "roles", null, 0, -1, FormGroup.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getFormGroup_Project(), theProjectPackage.getProject(), theProjectPackage.getProject_FormGroups(), "project",
				null, 0, 1, FormGroup.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES,
				!IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(formPanelEClass, FormPanel.class, "FormPanel", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getFormPanel_Name(), ecorePackage.getEString(), "name", null, 0, 1, FormPanel.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getFormPanel_ColumnCount(), ecorePackage.getEInt(), "columnCount", null, 0, 1, FormPanel.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getFormPanel_ColIndex(), ecorePackage.getEInt(), "colIndex", "1", 0, 1, FormPanel.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getFormPanel_RowIndex(), ecorePackage.getEInt(), "rowIndex", "1", 0, 1, FormPanel.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getFormPanel_Verticalspan(), ecorePackage.getEBoolean(), "verticalspan", "false", 0, 1, FormPanel.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getFormPanel_HorizontalSpan(), ecorePackage.getEBoolean(), "horizontalSpan", "false", 0, 1, FormPanel.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getFormPanel_FormGroup(), this.getFormGroup(), this.getFormGroup_Panels(), "formGroup", null, 0, 1,
				FormPanel.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEAttribute(getFormPanel_Label(), ecorePackage.getEString(), "label", null, 0, 1, FormPanel.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getFormPanel_Form(), this.getForm(), this.getForm_FormPanels(), "form", null, 0, 1, FormPanel.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getFormPanel_DrawBorder(), ecorePackage.getEBoolean(), "drawBorder", "true", 0, 1, FormPanel.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getFormPanel_BasePanel(), this.getFormPanel(), null, "basePanel", null, 0, 1, FormPanel.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getFormPanel_FormTable(), this.getFormTable(), this.getFormTable_FormPanel(), "formTable", null, 0, 1,
				FormPanel.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEReference(getFormPanel_Fields(), this.getFormField(), this.getFormField_Panel(), "fields", null, 0, -1, FormPanel.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEReference(getFormPanel_Actions(), this.getFormAction(), this.getFormAction_Panel(), "actions", null, 0, -1,
				FormPanel.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEReference(getFormPanel_DTO(), theDtoPackage.getDTOBean(), null, "dTO", null, 0, 1, FormPanel.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getFormPanel_BoundaryMethod(), theBoundaryPackage.getBoundaryMethod(), null, "boundaryMethod", null, 0, 1,
				FormPanel.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEReference(getFormPanel_Association(), theDomainPackage.getAbstractDomainAssociation(), null, "association", null, 0, 1,
				FormPanel.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);

		initEClass(formTableEClass, FormTable.class, "FormTable", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getFormTable_Name(), ecorePackage.getEString(), "name", null, 0, 1, FormTable.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getFormTable_ColIndex(), ecorePackage.getEInt(), "colIndex", "1", 0, 1, FormTable.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getFormTable_RowIndex(), ecorePackage.getEInt(), "rowIndex", "1", 0, 1, FormTable.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getFormTable_SpanCols(), ecorePackage.getEInt(), "spanCols", "1", 0, 1, FormTable.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getFormTable_SpanRows(), ecorePackage.getEInt(), "spanRows", "1", 0, 1, FormTable.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getFormTable_VerticalSpan(), ecorePackage.getEBoolean(), "verticalSpan", "true", 0, 1, FormTable.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getFormTable_HorizontalSpan(), ecorePackage.getEBoolean(), "horizontalSpan", "true", 0, 1, FormTable.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getFormTable_FormPanel(), this.getFormPanel(), this.getFormPanel_FormTable(), "formPanel", null, 0, 1,
				FormTable.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEReference(getFormTable_Fields(), this.getTableColumnField(), this.getTableColumnField_FormTable(), "fields", null, 0, -1,
				FormTable.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEReference(getFormTable_Association(), theDomainPackage.getAbstractDomainAssociation(), null, "association", null, 0, 1,
				FormTable.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);

		initEClass(tableColumnFieldEClass, TableColumnField.class, "TableColumnField", !IS_ABSTRACT, !IS_INTERFACE,
				IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getTableColumnField_ColIndex(), ecorePackage.getEInt(), "colIndex", "1", 0, 1, TableColumnField.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getTableColumnField_Visible(), ecorePackage.getEBoolean(), "visible", "true", 0, 1, TableColumnField.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getTableColumnField_Identifier(), ecorePackage.getEBoolean(), "identifier", "false", 0, 1,
				TableColumnField.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEReference(getTableColumnField_FormTable(), this.getFormTable(), this.getFormTable_Fields(), "formTable", null, 0, 1,
				TableColumnField.class, IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE,
				IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getTableColumnField_Width(), ecorePackage.getEInt(), "width", "100", 0, 1, TableColumnField.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getTableColumnField_Title(), ecorePackage.getEString(), "title", null, 0, 1, TableColumnField.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getTableColumnField_LovForm(), this.getForm(), null, "lovForm", null, 0, 1, TableColumnField.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getTableColumnField_FieldType(), this.getTableColumnFieldTypeEnumeration(), "fieldType", null, 0, 1,
				TableColumnField.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getTableColumnField_AssociationRef(), ecorePackage.getEBoolean(), "associationRef", null, 0, 1,
				TableColumnField.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getTableColumnField_Searchable(), ecorePackage.getEBoolean(), "searchable", "true", 0, 1,
				TableColumnField.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEReference(getTableColumnField_DTOAttribute(), theDtoPackage.getDTOBeanAttribute(), null, "dTOAttribute", null, 0, 1,
				TableColumnField.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE,
				IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(treeViewEClass, TreeView.class, "TreeView", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getTreeView_RootTreeItem(), this.getTreeViewItem(), null, "rootTreeItem", null, 0, 1, TreeView.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEReference(getTreeView_QuickSearchItems(), this.getTreeSearchItem(), null, "quickSearchItems", null, 0, -1,
				TreeView.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEReference(getTreeView_AdvancedSearchItems(), this.getTreeSearchItem(), null, "advancedSearchItems", null, 0, -1,
				TreeView.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEReference(getTreeView_CountMethod(), theBoundaryPackage.getBoundaryMethod(), null, "countMethod", null, 0, 1,
				TreeView.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEReference(getTreeView_RecursiveMethod(), theBoundaryPackage.getBoundaryMethod(), null, "recursiveMethod", null, 0, 1,
				TreeView.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);

		initEClass(treeViewItemEClass, TreeViewItem.class, "TreeViewItem", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getTreeViewItem_ParentItem(), this.getTreeViewItem(), this.getTreeViewItem_Children(), "parentItem", null, 0,
				1, TreeViewItem.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE,
				IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getTreeViewItem_Children(), this.getTreeViewItem(), this.getTreeViewItem_ParentItem(), "children", null, 0, -1,
				TreeViewItem.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE,
				IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getTreeViewItem_DataFetchMethod(), theBoundaryPackage.getBoundaryMethod(), null, "dataFetchMethod", null, 0, 1,
				TreeViewItem.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE,
				IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getTreeViewItem_DropMethod(), theBoundaryPackage.getBoundaryMethod(), null, "dropMethod", null, 0, 1,
				TreeViewItem.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE,
				IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getTreeViewItem_DisplayAttributes(), theDtoPackage.getDTOBeanAttribute(), null, "displayAttributes", null, 0,
				-1, TreeViewItem.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE,
				IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getTreeViewItem_Association(), theDomainPackage.getAbstractDomainAssociation(), null, "association", null, 0,
				1, TreeViewItem.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE,
				IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getTreeViewItem_Nodes(), this.getTreeNode(), null, "nodes", null, 0, -1, TreeViewItem.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getTreeViewItem_Label(), ecorePackage.getEString(), "label", null, 0, 1, TreeViewItem.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getTreeViewItem_ItemDTO(), theDtoPackage.getDTOBean(), null, "itemDTO", null, 0, 1, TreeViewItem.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEReference(getTreeViewItem_InvisibleAttributes(), theDtoPackage.getDTOBeanAttribute(), null, "invisibleAttributes", null,
				0, -1, TreeViewItem.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE,
				IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(treeSearchItemEClass, TreeSearchItem.class, "TreeSearchItem", !IS_ABSTRACT, !IS_INTERFACE,
				IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getTreeSearchItem_Label(), ecorePackage.getEString(), "label", null, 0, 1, TreeSearchItem.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getTreeSearchItem_DTOAttribute(), theDtoPackage.getDTOBeanAttribute(), null, "dTOAttribute", null, 0, 1,
				TreeSearchItem.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE,
				IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(treeNodeEClass, TreeNode.class, "TreeNode", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getTreeNode_Label(), ecorePackage.getEString(), "label", null, 0, 1, TreeNode.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getTreeNode_DTOAttribute(), theDtoPackage.getDTOBeanAttribute(), null, "dTOAttribute", null, 0, 1,
				TreeNode.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);

		// Initialize enums and add enum literals
		initEEnum(formFieldTypeEnumerationEEnum, FormFieldTypeEnumeration.class, "FormFieldTypeEnumeration");
		addEEnumLiteral(formFieldTypeEnumerationEEnum, FormFieldTypeEnumeration.SIMPLE_TEXT);
		addEEnumLiteral(formFieldTypeEnumerationEEnum, FormFieldTypeEnumeration.LABEL);
		addEEnumLiteral(formFieldTypeEnumerationEEnum, FormFieldTypeEnumeration.CHECKBOX);
		addEEnumLiteral(formFieldTypeEnumerationEEnum, FormFieldTypeEnumeration.DATE_TIME);
		addEEnumLiteral(formFieldTypeEnumerationEEnum, FormFieldTypeEnumeration.MULTI_LINE_TEXT);
		addEEnumLiteral(formFieldTypeEnumerationEEnum, FormFieldTypeEnumeration.COMBOBOX);
		addEEnumLiteral(formFieldTypeEnumerationEEnum, FormFieldTypeEnumeration.LOV);
		addEEnumLiteral(formFieldTypeEnumerationEEnum, FormFieldTypeEnumeration.DATE);
		addEEnumLiteral(formFieldTypeEnumerationEEnum, FormFieldTypeEnumeration.LIST);
		addEEnumLiteral(formFieldTypeEnumerationEEnum, FormFieldTypeEnumeration.SEARCHABLE_LIST);
		addEEnumLiteral(formFieldTypeEnumerationEEnum, FormFieldTypeEnumeration.SELECTION_BY_PARENT_FORM);
		addEEnumLiteral(formFieldTypeEnumerationEEnum, FormFieldTypeEnumeration.SELECTION_BY_SECURITY_DTO);
		addEEnumLiteral(formFieldTypeEnumerationEEnum, FormFieldTypeEnumeration.PROPOSAL_TEXT);
		addEEnumLiteral(formFieldTypeEnumerationEEnum, FormFieldTypeEnumeration.ENUM_COMBOBOX);
		addEEnumLiteral(formFieldTypeEnumerationEEnum, FormFieldTypeEnumeration.DOCUMENT_SIZE_FIELD);
		addEEnumLiteral(formFieldTypeEnumerationEEnum, FormFieldTypeEnumeration.MULTI_LINE_LABEL);
		addEEnumLiteral(formFieldTypeEnumerationEEnum, FormFieldTypeEnumeration.WEB_LINK);
		addEEnumLiteral(formFieldTypeEnumerationEEnum, FormFieldTypeEnumeration.MAIL_LINK);
		addEEnumLiteral(formFieldTypeEnumerationEEnum, FormFieldTypeEnumeration.FORM_LINK);
		addEEnumLiteral(formFieldTypeEnumerationEEnum, FormFieldTypeEnumeration.SELECTION_BY_CLIENT);
		addEEnumLiteral(formFieldTypeEnumerationEEnum, FormFieldTypeEnumeration.ELEMENT_COLLECTION_EDITOR);

		initEEnum(formTypeEnumerationEEnum, FormTypeEnumeration.class, "FormTypeEnumeration");
		addEEnumLiteral(formTypeEnumerationEEnum, FormTypeEnumeration.UPDATE);
		addEEnumLiteral(formTypeEnumerationEEnum, FormTypeEnumeration.READONLY);
		addEEnumLiteral(formTypeEnumerationEEnum, FormTypeEnumeration.CREATE);
		addEEnumLiteral(formTypeEnumerationEEnum, FormTypeEnumeration.SEARCHABLE_VIEW);
		addEEnumLiteral(formTypeEnumerationEEnum, FormTypeEnumeration.SIMPLE_VIEW);
		addEEnumLiteral(formTypeEnumerationEEnum, FormTypeEnumeration.ADD);
		addEEnumLiteral(formTypeEnumerationEEnum, FormTypeEnumeration.LOV);
		addEEnumLiteral(formTypeEnumerationEEnum, FormTypeEnumeration.GRID);
		addEEnumLiteral(formTypeEnumerationEEnum, FormTypeEnumeration.TREE_VIEW);

		initEEnum(tableColumnFieldTypeEnumerationEEnum, TableColumnFieldTypeEnumeration.class, "TableColumnFieldTypeEnumeration");
		addEEnumLiteral(tableColumnFieldTypeEnumerationEEnum, TableColumnFieldTypeEnumeration.STRING);
		addEEnumLiteral(tableColumnFieldTypeEnumerationEEnum, TableColumnFieldTypeEnumeration.INTEGER);
		addEEnumLiteral(tableColumnFieldTypeEnumerationEEnum, TableColumnFieldTypeEnumeration.DOUBLE);
		addEEnumLiteral(tableColumnFieldTypeEnumerationEEnum, TableColumnFieldTypeEnumeration.BOOLEAN);
		addEEnumLiteral(tableColumnFieldTypeEnumerationEEnum, TableColumnFieldTypeEnumeration.DATE);
		addEEnumLiteral(tableColumnFieldTypeEnumerationEEnum, TableColumnFieldTypeEnumeration.GREGORIAN_CALENDAR);
		addEEnumLiteral(tableColumnFieldTypeEnumerationEEnum, TableColumnFieldTypeEnumeration.ENUM);
		addEEnumLiteral(tableColumnFieldTypeEnumerationEEnum, TableColumnFieldTypeEnumeration.FLOAT);
		addEEnumLiteral(tableColumnFieldTypeEnumerationEEnum, TableColumnFieldTypeEnumeration.LONG);
		addEEnumLiteral(tableColumnFieldTypeEnumerationEEnum, TableColumnFieldTypeEnumeration.CHAR);
		addEEnumLiteral(tableColumnFieldTypeEnumerationEEnum, TableColumnFieldTypeEnumeration.BIG_DECIMAL);
		addEEnumLiteral(tableColumnFieldTypeEnumerationEEnum, TableColumnFieldTypeEnumeration.LOCAL_DATE);
		addEEnumLiteral(tableColumnFieldTypeEnumerationEEnum, TableColumnFieldTypeEnumeration.LOCAL_DATE_TIME);
		addEEnumLiteral(tableColumnFieldTypeEnumerationEEnum, TableColumnFieldTypeEnumeration.UUID_BINARY);
		addEEnumLiteral(tableColumnFieldTypeEnumerationEEnum, TableColumnFieldTypeEnumeration.UUID_STRING);

		initEEnum(actionTypeEEnum, ActionType.class, "ActionType");
		addEEnumLiteral(actionTypeEEnum, ActionType.CREATE);
		addEEnumLiteral(actionTypeEEnum, ActionType.DIRECT_UPLOAD);
		addEEnumLiteral(actionTypeEEnum, ActionType.INDIRECT_UPLOAD);
		addEEnumLiteral(actionTypeEEnum, ActionType.DOWNLOAD);
		addEEnumLiteral(actionTypeEEnum, ActionType.POSITVE_DECISION);
		addEEnumLiteral(actionTypeEEnum, ActionType.NEGATIVE_DECISION);
		addEEnumLiteral(actionTypeEEnum, ActionType.UNDEF_DECISION);
		addEEnumLiteral(actionTypeEEnum, ActionType.UPDATE);
		addEEnumLiteral(actionTypeEEnum, ActionType.READ);
		addEEnumLiteral(actionTypeEEnum, ActionType.DELETE);
		addEEnumLiteral(actionTypeEEnum, ActionType.DOWNLOAD_EXPORT);
		addEEnumLiteral(actionTypeEEnum, ActionType.UPLOAD_IMPORT);
		addEEnumLiteral(actionTypeEEnum, ActionType.COPY);

		// Create resource
		createResource(eNS_URI);
	}

}
