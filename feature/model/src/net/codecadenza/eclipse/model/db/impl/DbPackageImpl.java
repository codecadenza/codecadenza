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
package net.codecadenza.eclipse.model.db.impl;

import net.codecadenza.eclipse.model.boundary.BoundaryPackage;
import net.codecadenza.eclipse.model.boundary.impl.BoundaryPackageImpl;
import net.codecadenza.eclipse.model.client.ClientPackage;
import net.codecadenza.eclipse.model.client.impl.ClientPackageImpl;
import net.codecadenza.eclipse.model.db.DBColumn;
import net.codecadenza.eclipse.model.db.DBColumnType;
import net.codecadenza.eclipse.model.db.DBIndex;
import net.codecadenza.eclipse.model.db.DBTable;
import net.codecadenza.eclipse.model.db.DBVendorGroupEnumeration;
import net.codecadenza.eclipse.model.db.Database;
import net.codecadenza.eclipse.model.db.DbFactory;
import net.codecadenza.eclipse.model.db.DbPackage;
import net.codecadenza.eclipse.model.db.ForeignKey;
import net.codecadenza.eclipse.model.db.IdentifierStyleEnumeration;
import net.codecadenza.eclipse.model.db.PrimaryKey;
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
public class DbPackageImpl extends EPackageImpl implements DbPackage {
	/**
	 * @generated
	 */
	private EClass dbColumnEClass;

	/**
	 * @generated
	 */
	private EClass dbColumnTypeEClass;

	/**
	 * @generated
	 */
	private EClass dbIndexEClass;

	/**
	 * @generated
	 */
	private EClass dbTableEClass;

	/**
	 * @generated
	 */
	private EClass databaseEClass;

	/**
	 * @generated
	 */
	private EClass foreignKeyEClass;

	/**
	 * @generated
	 */
	private EClass primaryKeyEClass;

	/**
	 * @generated
	 */
	private EEnum dbVendorGroupEnumerationEEnum;

	/**
	 * @generated
	 */
	private EEnum identifierStyleEnumerationEEnum;

	/**
	 * Create an instance of the model <b>Package</b>, registered with {@link org.eclipse.emf.ecore.EPackage.Registry
	 * EPackage.Registry} by the package URI value.
	 * <p>
	 * Note: The correct way to create the package is via the static factory method {@link #init init()}, which also performs
	 * initialization of the package, or returns the registered package, if one already exists.
	 * </p>
	 * @see org.eclipse.emf.ecore.EPackage.Registry
	 * @see net.codecadenza.eclipse.model.db.DbPackage#eNS_URI
	 * @see #init()
	 * @generated
	 */
	private DbPackageImpl() {
		super(eNS_URI, DbFactory.eINSTANCE);
	}

	/**
	 * @generated
	 */
	private static boolean isInited;

	/**
	 * Create, register and initialize the <b>Package</b> for this model, and for any others upon which it depends. This method is
	 * used to initialize {@link DbPackage#eINSTANCE} when that field is accessed. Clients should not invoke it directly. Instead,
	 * they should simply access that field to obtain the package.
	 * @see #eNS_URI
	 * @see #createPackageContents()
	 * @see #initializePackageContents()
	 * @return the initialized DB package
	 * @generated
	 */
	public static DbPackage init() {
		if (isInited)
			return (DbPackage) EPackage.Registry.INSTANCE.getEPackage(DbPackage.eNS_URI);

		// Obtain or create and register package
		final var theDbPackage = (DbPackageImpl) (EPackage.Registry.INSTANCE.get(eNS_URI) instanceof DbPackageImpl
				? EPackage.Registry.INSTANCE.get(eNS_URI) : new DbPackageImpl());

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
		theDbPackage.createPackageContents();
		theBoundaryPackage.createPackageContents();
		theClientPackage.createPackageContents();
		theRepositoryPackage.createPackageContents();
		theDomainPackage.createPackageContents();
		theDtoPackage.createPackageContents();
		theJavaPackage.createPackageContents();
		theProjectPackage.createPackageContents();
		theExchangePackage.createPackageContents();
		theServicePackage.createPackageContents();
		theMappingPackage.createPackageContents();

		// Initialize created meta-data
		theDbPackage.initializePackageContents();
		theBoundaryPackage.initializePackageContents();
		theClientPackage.initializePackageContents();
		theRepositoryPackage.initializePackageContents();
		theDomainPackage.initializePackageContents();
		theDtoPackage.initializePackageContents();
		theJavaPackage.initializePackageContents();
		theProjectPackage.initializePackageContents();
		theExchangePackage.initializePackageContents();
		theServicePackage.initializePackageContents();
		theMappingPackage.initializePackageContents();

		// Mark meta-data to indicate it can't be changed
		theDbPackage.freeze();

		// Update the registry and return the package
		EPackage.Registry.INSTANCE.put(DbPackage.eNS_URI, theDbPackage);
		return theDbPackage;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDBColumn()
	 * @generated
	 */
	@Override
	public EClass getDBColumn() {
		return dbColumnEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDBColumn_Name()
	 * @generated
	 */
	@Override
	public EAttribute getDBColumn_Name() {
		return (EAttribute) dbColumnEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDBColumn_Length()
	 * @generated
	 */
	@Override
	public EAttribute getDBColumn_Length() {
		return (EAttribute) dbColumnEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDBColumn_Nullable()
	 * @generated
	 */
	@Override
	public EAttribute getDBColumn_Nullable() {
		return (EAttribute) dbColumnEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDBColumn_Precision()
	 */
	@Override
	public EAttribute getDBColumn_Precision() {
		return (EAttribute) dbColumnEClass.getEStructuralFeatures().get(3);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDBColumn_Scale()
	 * @generated
	 */
	@Override
	public EAttribute getDBColumn_Scale() {
		return (EAttribute) dbColumnEClass.getEStructuralFeatures().get(4);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDBColumn_DatabaseTable()
	 * @generated
	 */
	@Override
	public EReference getDBColumn_DatabaseTable() {
		return (EReference) dbColumnEClass.getEStructuralFeatures().get(5);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDBColumn_ColumnType()
	 * @generated
	 */
	@Override
	public EReference getDBColumn_ColumnType() {
		return (EReference) dbColumnEClass.getEStructuralFeatures().get(6);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDBColumnType()
	 * @generated
	 */
	@Override
	public EClass getDBColumnType() {
		return dbColumnTypeEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDBColumnType_Name()
	 * @generated
	 */
	@Override
	public EAttribute getDBColumnType_Name() {
		return (EAttribute) dbColumnTypeEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDBColumnType_OmitSizeInformation()
	 * @generated
	 */
	@Override
	public EAttribute getDBColumnType_OmitSizeInformation() {
		return (EAttribute) dbColumnTypeEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDBColumnType_JavaTypes()
	 * @generated
	 */
	@Override
	public EReference getDBColumnType_JavaTypes() {
		return (EReference) dbColumnTypeEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDBIndex()
	 * @generated
	 */
	@Override
	public EClass getDBIndex() {
		return dbIndexEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDBIndex_Name()
	 * @generated
	 */
	@Override
	public EAttribute getDBIndex_Name() {
		return (EAttribute) dbIndexEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDBIndex_Table()
	 * @generated
	 */
	@Override
	public EReference getDBIndex_Table() {
		return (EReference) dbIndexEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDBIndex_Unique()
	 * @generated
	 */
	@Override
	public EAttribute getDBIndex_Unique() {
		return (EAttribute) dbIndexEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDBIndex_Columns()
	 * @generated
	 */
	@Override
	public EReference getDBIndex_Columns() {
		return (EReference) dbIndexEClass.getEStructuralFeatures().get(3);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDBTable()
	 * @generated
	 */
	@Override
	public EClass getDBTable() {
		return dbTableEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDBTable_Name()
	 * @generated
	 */
	@Override
	public EAttribute getDBTable_Name() {
		return (EAttribute) dbTableEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDBTable_Database()
	 * @generated
	 */
	@Override
	public EReference getDBTable_Database() {
		return (EReference) dbTableEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDBTable_Columns()
	 * @generated
	 */
	@Override
	public EReference getDBTable_Columns() {
		return (EReference) dbTableEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDBTable_PrimaryKey()
	 * @generated
	 */
	@Override
	public EReference getDBTable_PrimaryKey() {
		return (EReference) dbTableEClass.getEStructuralFeatures().get(3);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDBTable_ForeignKeys()
	 * @generated
	 */
	@Override
	public EReference getDBTable_ForeignKeys() {
		return (EReference) dbTableEClass.getEStructuralFeatures().get(4);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDBTable_Indexes()
	 * @generated
	 */
	@Override
	public EReference getDBTable_Indexes() {
		return (EReference) dbTableEClass.getEStructuralFeatures().get(5);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDBTable_SchemaName()
	 * @generated
	 */
	@Override
	public EAttribute getDBTable_SchemaName() {
		return (EAttribute) dbTableEClass.getEStructuralFeatures().get(6);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDBTable_CatalogName()
	 * @generated
	 */
	@Override
	public EAttribute getDBTable_CatalogName() {
		return (EAttribute) dbTableEClass.getEStructuralFeatures().get(7);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDatabase()
	 * @generated
	 */
	@Override
	public EClass getDatabase() {
		return databaseEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDatabase_DatabaseTables()
	 * @generated
	 */
	@Override
	public EReference getDatabase_DatabaseTables() {
		return (EReference) databaseEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDatabase_Project()
	 * @generated
	 */
	@Override
	public EReference getDatabase_Project() {
		return (EReference) databaseEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDatabase_SchemaName()
	 * @generated
	 */
	@Override
	public EAttribute getDatabase_SchemaName() {
		return (EAttribute) databaseEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDatabase_CatalogName()
	 * @generated
	 */
	@Override
	public EAttribute getDatabase_CatalogName() {
		return (EAttribute) databaseEClass.getEStructuralFeatures().get(3);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDatabase_IdentifierRegEx()
	 * @generated
	 */
	@Override
	public EAttribute getDatabase_IdentifierRegEx() {
		return (EAttribute) databaseEClass.getEStructuralFeatures().get(4);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDatabase_IdentifierStyle()
	 * @generated
	 */
	@Override
	public EAttribute getDatabase_IdentifierStyle() {
		return (EAttribute) databaseEClass.getEStructuralFeatures().get(5);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDatabase_MaxIdentifierLength()
	 * @generated
	 */
	@Override
	public EAttribute getDatabase_MaxIdentifierLength() {
		return (EAttribute) databaseEClass.getEStructuralFeatures().get(6);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDatabase_HibernateDialect()
	 * @generated
	 */
	@Override
	public EAttribute getDatabase_HibernateDialect() {
		return (EAttribute) databaseEClass.getEStructuralFeatures().get(7);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDatabase_EclipseLinkTargetDBName()
	 * @generated
	 */
	@Override
	public EAttribute getDatabase_EclipseLinkTargetDBName() {
		return (EAttribute) databaseEClass.getEStructuralFeatures().get(8);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDatabase_ReservedWords()
	 * @generated
	 */
	@Override
	public EAttribute getDatabase_ReservedWords() {
		return (EAttribute) databaseEClass.getEStructuralFeatures().get(9);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDatabase_SupportsIdentityColumn()
	 * @generated
	 */
	@Override
	public EAttribute getDatabase_SupportsIdentityColumn() {
		return (EAttribute) databaseEClass.getEStructuralFeatures().get(10);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDatabase_SupportsSequence()
	 * @generated
	 */
	@Override
	public EAttribute getDatabase_SupportsSequence() {
		return (EAttribute) databaseEClass.getEStructuralFeatures().get(11);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDatabase_VendorGroup()
	 * @generated
	 */
	@Override
	public EAttribute getDatabase_VendorGroup() {
		return (EAttribute) databaseEClass.getEStructuralFeatures().get(12);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDatabase_AllSupportedColumnTypes()
	 * @generated
	 */
	@Override
	public EReference getDatabase_AllSupportedColumnTypes() {
		return (EReference) databaseEClass.getEStructuralFeatures().get(13);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getForeignKey()
	 * @generated
	 */
	@Override
	public EClass getForeignKey() {
		return foreignKeyEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getForeignKey_Name()
	 * @generated
	 */
	@Override
	public EAttribute getForeignKey_Name() {
		return (EAttribute) foreignKeyEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getForeignKey_Table()
	 * @generated
	 */
	@Override
	public EReference getForeignKey_Table() {
		return (EReference) foreignKeyEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getForeignKey_Column()
	 * @generated
	 */
	@Override
	public EReference getForeignKey_Column() {
		return (EReference) foreignKeyEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getForeignKey_ReferencedColumn()
	 * @generated
	 */
	@Override
	public EReference getForeignKey_ReferencedColumn() {
		return (EReference) foreignKeyEClass.getEStructuralFeatures().get(3);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getPrimaryKey()
	 * @generated
	 */
	@Override
	public EClass getPrimaryKey() {
		return primaryKeyEClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getPrimaryKey_Name()
	 * @generated
	 */
	@Override
	public EAttribute getPrimaryKey_Name() {
		return (EAttribute) primaryKeyEClass.getEStructuralFeatures().get(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getPrimaryKey_Table()
	 * @generated
	 */
	@Override
	public EReference getPrimaryKey_Table() {
		return (EReference) primaryKeyEClass.getEStructuralFeatures().get(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getPrimaryKey_Column()
	 * @generated
	 */
	@Override
	public EReference getPrimaryKey_Column() {
		return (EReference) primaryKeyEClass.getEStructuralFeatures().get(2);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDBVendorGroupEnumeration()
	 * @generated
	 */
	@Override
	public EEnum getDBVendorGroupEnumeration() {
		return dbVendorGroupEnumerationEEnum;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getIdentifierStyleEnumeration()
	 * @generated
	 */
	@Override
	public EEnum getIdentifierStyleEnumeration() {
		return identifierStyleEnumerationEEnum;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.db.DbPackage#getDbFactory()
	 * @generated
	 */
	@Override
	public DbFactory getDbFactory() {
		return (DbFactory) getEFactoryInstance();
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
		dbColumnEClass = createEClass(DB_COLUMN);
		createEAttribute(dbColumnEClass, DB_COLUMN__NAME);
		createEAttribute(dbColumnEClass, DB_COLUMN__LENGTH);
		createEAttribute(dbColumnEClass, DB_COLUMN__NULLABLE);
		createEAttribute(dbColumnEClass, DB_COLUMN__PRECISION);
		createEAttribute(dbColumnEClass, DB_COLUMN__SCALE);
		createEReference(dbColumnEClass, DB_COLUMN__DATABASE_TABLE);
		createEReference(dbColumnEClass, DB_COLUMN__COLUMN_TYPE);

		dbColumnTypeEClass = createEClass(DB_COLUMN_TYPE);
		createEReference(dbColumnTypeEClass, DB_COLUMN_TYPE__JAVA_TYPES);
		createEAttribute(dbColumnTypeEClass, DB_COLUMN_TYPE__NAME);
		createEAttribute(dbColumnTypeEClass, DB_COLUMN_TYPE__OMIT_SIZE_INFORMATION);

		dbIndexEClass = createEClass(DB_INDEX);
		createEAttribute(dbIndexEClass, DB_INDEX__NAME);
		createEReference(dbIndexEClass, DB_INDEX__TABLE);
		createEAttribute(dbIndexEClass, DB_INDEX__UNIQUE);
		createEReference(dbIndexEClass, DB_INDEX__COLUMNS);

		dbTableEClass = createEClass(DB_TABLE);
		createEAttribute(dbTableEClass, DB_TABLE__NAME);
		createEReference(dbTableEClass, DB_TABLE__DATABASE);
		createEReference(dbTableEClass, DB_TABLE__COLUMNS);
		createEReference(dbTableEClass, DB_TABLE__PRIMARY_KEY);
		createEReference(dbTableEClass, DB_TABLE__FOREIGN_KEYS);
		createEReference(dbTableEClass, DB_TABLE__INDEXES);
		createEAttribute(dbTableEClass, DB_TABLE__SCHEMA_NAME);
		createEAttribute(dbTableEClass, DB_TABLE__CATALOG_NAME);

		databaseEClass = createEClass(DATABASE);
		createEReference(databaseEClass, DATABASE__DATABASE_TABLES);
		createEReference(databaseEClass, DATABASE__PROJECT);
		createEAttribute(databaseEClass, DATABASE__SCHEMA_NAME);
		createEAttribute(databaseEClass, DATABASE__CATALOG_NAME);
		createEAttribute(databaseEClass, DATABASE__IDENTIFIER_REG_EX);
		createEAttribute(databaseEClass, DATABASE__IDENTIFIER_STYLE);
		createEAttribute(databaseEClass, DATABASE__MAX_IDENTIFIER_LENGTH);
		createEAttribute(databaseEClass, DATABASE__HIBERNATE_DIALECT);
		createEAttribute(databaseEClass, DATABASE__ECLIPSE_LINK_TARGET_DB_NAME);
		createEAttribute(databaseEClass, DATABASE__RESERVED_WORDS);
		createEAttribute(databaseEClass, DATABASE__SUPPORTS_IDENTITY_COLUMN);
		createEAttribute(databaseEClass, DATABASE__SUPPORTS_SEQUENCE);
		createEAttribute(databaseEClass, DATABASE__VENDOR_GROUP);
		createEReference(databaseEClass, DATABASE__ALL_SUPPORTED_COLUMN_TYPES);

		foreignKeyEClass = createEClass(FOREIGN_KEY);
		createEAttribute(foreignKeyEClass, FOREIGN_KEY__NAME);
		createEReference(foreignKeyEClass, FOREIGN_KEY__TABLE);
		createEReference(foreignKeyEClass, FOREIGN_KEY__COLUMN);
		createEReference(foreignKeyEClass, FOREIGN_KEY__REFERENCED_COLUMN);

		primaryKeyEClass = createEClass(PRIMARY_KEY);
		createEAttribute(primaryKeyEClass, PRIMARY_KEY__NAME);
		createEReference(primaryKeyEClass, PRIMARY_KEY__TABLE);
		createEReference(primaryKeyEClass, PRIMARY_KEY__COLUMN);

		// Create enums
		dbVendorGroupEnumerationEEnum = createEEnum(DB_VENDOR_GROUP_ENUMERATION);
		identifierStyleEnumerationEEnum = createEEnum(IDENTIFIER_STYLE_ENUMERATION);
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

		// Initialize classes and features; add operations and parameters
		initEClass(dbColumnEClass, DBColumn.class, "DBColumn", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getDBColumn_Name(), ecorePackage.getEString(), "name", null, 0, 1, DBColumn.class, !IS_TRANSIENT, !IS_VOLATILE,
				IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getDBColumn_Length(), ecorePackage.getEInt(), "length", null, 0, 1, DBColumn.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getDBColumn_Nullable(), ecorePackage.getEBoolean(), "nullable", null, 0, 1, DBColumn.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getDBColumn_Precision(), ecorePackage.getEInt(), "precision", null, 0, 1, DBColumn.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getDBColumn_Scale(), ecorePackage.getEInt(), "scale", null, 0, 1, DBColumn.class, !IS_TRANSIENT, !IS_VOLATILE,
				IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getDBColumn_DatabaseTable(), this.getDBTable(), this.getDBTable_Columns(), "databaseTable", null, 0, 1,
				DBColumn.class, IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEReference(getDBColumn_ColumnType(), this.getDBColumnType(), null, "columnType", null, 0, 1, DBColumn.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);

		initEClass(dbColumnTypeEClass, DBColumnType.class, "DBColumnType", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getDBColumnType_JavaTypes(), theJavaPackage.getJavaType(), null, "javaTypes", null, 0, -1, DBColumnType.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getDBColumnType_Name(), ecorePackage.getEString(), "name", null, 0, 1, DBColumnType.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getDBColumnType_OmitSizeInformation(), ecorePackage.getEBoolean(), "omitSizeInformation", null, 0, 1,
				DBColumnType.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);

		initEClass(dbIndexEClass, DBIndex.class, "DBIndex", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getDBIndex_Name(), ecorePackage.getEString(), "name", null, 0, 1, DBIndex.class, !IS_TRANSIENT, !IS_VOLATILE,
				IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getDBIndex_Table(), this.getDBTable(), this.getDBTable_Indexes(), "table", null, 0, 1, DBIndex.class,
				IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getDBIndex_Unique(), ecorePackage.getEBoolean(), "unique", null, 0, 1, DBIndex.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getDBIndex_Columns(), this.getDBColumn(), null, "columns", null, 0, -1, DBIndex.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(dbTableEClass, DBTable.class, "DBTable", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getDBTable_Name(), ecorePackage.getEString(), "name", null, 0, 1, DBTable.class, !IS_TRANSIENT, !IS_VOLATILE,
				IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getDBTable_Database(), this.getDatabase(), this.getDatabase_DatabaseTables(), "database", null, 0, 1,
				DBTable.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEReference(getDBTable_Columns(), this.getDBColumn(), this.getDBColumn_DatabaseTable(), "columns", null, 0, -1,
				DBTable.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEReference(getDBTable_PrimaryKey(), this.getPrimaryKey(), this.getPrimaryKey_Table(), "primaryKey", null, 0, 1,
				DBTable.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEReference(getDBTable_ForeignKeys(), this.getForeignKey(), this.getForeignKey_Table(), "foreignKeys", null, 0, -1,
				DBTable.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEReference(getDBTable_Indexes(), this.getDBIndex(), this.getDBIndex_Table(), "indexes", null, 0, -1, DBTable.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEAttribute(getDBTable_SchemaName(), ecorePackage.getEString(), "schemaName", null, 0, 1, DBTable.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getDBTable_CatalogName(), ecorePackage.getEString(), "catalogName", null, 0, 1, DBTable.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(databaseEClass, Database.class, "Database", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getDatabase_DatabaseTables(), this.getDBTable(), this.getDBTable_Database(), "databaseTables", null, 0, -1,
				Database.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);
		initEReference(getDatabase_Project(), theProjectPackage.getProject(), theProjectPackage.getProject_Database(), "project",
				null, 0, 1, Database.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, !IS_RESOLVE_PROXIES,
				!IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getDatabase_SchemaName(), ecorePackage.getEString(), "schemaName", null, 0, 1, Database.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getDatabase_CatalogName(), ecorePackage.getEString(), "catalogName", null, 0, 1, Database.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getDatabase_IdentifierRegEx(), ecorePackage.getEString(), "identifierRegEx", null, 0, 1, Database.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getDatabase_IdentifierStyle(), this.getIdentifierStyleEnumeration(), "identifierStyle", null, 0, 1,
				Database.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getDatabase_MaxIdentifierLength(), ecorePackage.getEInt(), "maxIdentifierLength", null, 0, 1, Database.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getDatabase_HibernateDialect(), ecorePackage.getEString(), "hibernateDialect", null, 0, 1, Database.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getDatabase_EclipseLinkTargetDBName(), ecorePackage.getEString(), "eclipseLinkTargetDBName", null, 0, 1,
				Database.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getDatabase_ReservedWords(), ecorePackage.getEString(), "reservedWords", null, 0, 1, Database.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getDatabase_SupportsIdentityColumn(), ecorePackage.getEBoolean(), "supportsIdentityColumn", null, 0, 1,
				Database.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getDatabase_SupportsSequence(), ecorePackage.getEBoolean(), "supportsSequence", null, 0, 1, Database.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getDatabase_VendorGroup(), this.getDBVendorGroupEnumeration(), "vendorGroup", null, 0, 1, Database.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getDatabase_AllSupportedColumnTypes(), this.getDBColumnType(), null, "allSupportedColumnTypes", null, 0, -1,
				Database.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE,
				!IS_DERIVED, IS_ORDERED);

		initEClass(foreignKeyEClass, ForeignKey.class, "ForeignKey", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getForeignKey_Name(), ecorePackage.getEString(), "name", null, 0, 1, ForeignKey.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getForeignKey_Table(), this.getDBTable(), this.getDBTable_ForeignKeys(), "table", null, 0, 1, ForeignKey.class,
				IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEReference(getForeignKey_Column(), this.getDBColumn(), null, "column", null, 0, 1, ForeignKey.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getForeignKey_ReferencedColumn(), this.getDBColumn(), null, "referencedColumn", null, 0, 1, ForeignKey.class,
				!IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);

		initEClass(primaryKeyEClass, PrimaryKey.class, "PrimaryKey", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getPrimaryKey_Name(), ecorePackage.getEString(), "name", null, 0, 1, PrimaryKey.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getPrimaryKey_Table(), this.getDBTable(), this.getDBTable_PrimaryKey(), "table", null, 0, 1, PrimaryKey.class,
				IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED,
				IS_ORDERED);
		initEReference(getPrimaryKey_Column(), this.getDBColumn(), null, "column", null, 0, 1, PrimaryKey.class, !IS_TRANSIENT,
				!IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		// Initialize enums and add enum literals
		initEEnum(dbVendorGroupEnumerationEEnum, DBVendorGroupEnumeration.class, "DBVendorGroupEnumeration");
		addEEnumLiteral(dbVendorGroupEnumerationEEnum, DBVendorGroupEnumeration.MYSQL);
		addEEnumLiteral(dbVendorGroupEnumerationEEnum, DBVendorGroupEnumeration.ORACLE);
		addEEnumLiteral(dbVendorGroupEnumerationEEnum, DBVendorGroupEnumeration.H2);
		addEEnumLiteral(dbVendorGroupEnumerationEEnum, DBVendorGroupEnumeration.H2_EMBEDDED);
		addEEnumLiteral(dbVendorGroupEnumerationEEnum, DBVendorGroupEnumeration.POSTGRESQL);
		addEEnumLiteral(dbVendorGroupEnumerationEEnum, DBVendorGroupEnumeration.MSSQL);

		initEEnum(identifierStyleEnumerationEEnum, IdentifierStyleEnumeration.class, "IdentifierStyleEnumeration");
		addEEnumLiteral(identifierStyleEnumerationEEnum, IdentifierStyleEnumeration.UPPERCASE);
		addEEnumLiteral(identifierStyleEnumerationEEnum, IdentifierStyleEnumeration.LOWERCASE);
		addEEnumLiteral(identifierStyleEnumerationEEnum, IdentifierStyleEnumeration.CASE_SENSITIVE);

		// Create resource
		createResource(eNS_URI);
	}

}
