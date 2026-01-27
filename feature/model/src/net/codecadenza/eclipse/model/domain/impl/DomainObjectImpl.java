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
package net.codecadenza.eclipse.model.domain.impl;

import static net.codecadenza.eclipse.shared.Constants.LISTENER_SUFFIX;
import static net.codecadenza.eclipse.shared.Constants.SUB_PACKAGE_LISTENER;

import java.util.ArrayList;
import java.util.Collection;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.List;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.db.DBColumn;
import net.codecadenza.eclipse.model.db.DBTable;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.AssociationTagEnumeration;
import net.codecadenza.eclipse.model.domain.AttributeTagEnumeration;
import net.codecadenza.eclipse.model.domain.DiscriminatorColumnTypeEnumeration;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainInheritance;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.DomainPackage;
import net.codecadenza.eclipse.model.domain.DomainTagEnumeration;
import net.codecadenza.eclipse.model.domain.EnumAssociation;
import net.codecadenza.eclipse.model.domain.IDGenerator;
import net.codecadenza.eclipse.model.domain.InheritanceTypeEnumeration;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.domain.OneToOneAssociation;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.java.MethodParameter;
import net.codecadenza.eclipse.model.java.impl.JavaTypeImpl;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.repository.Repository;
import net.codecadenza.eclipse.model.repository.RepositoryMethod;
import net.codecadenza.eclipse.model.repository.RepositoryMethodParameter;
import net.codecadenza.eclipse.model.repository.RepositoryMethodTypeEnumeration;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.util.BasicEList;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.util.EObjectContainmentWithInverseEList;
import org.eclipse.emf.ecore.util.EObjectResolvingEList;
import org.eclipse.emf.ecore.util.InternalEList;

/**
 * An implementation of the model object '<em><b>Domain Object</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.DomainObjectImpl#getLabel <em>Label</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.DomainObjectImpl#getNamePlural <em>Name Plural</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.DomainObjectImpl#getLabelPlural <em>Label Plural</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.DomainObjectImpl#getDiscriminatorValue <em>Discriminator Value</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.DomainObjectImpl#getDiscriminatorColumnType <em>Discriminator Column
 * Type</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.DomainObjectImpl#getInheritanceType <em>Inheritance Type</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.DomainObjectImpl#isPropertyAccess <em>Property Access</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.DomainObjectImpl#isAbstract <em>Abstract</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.DomainObjectImpl#isMappedSuperClass <em>Mapped Super Class</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.DomainObjectImpl#getParent <em>Parent</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.DomainObjectImpl#getIDGenerator <em>ID Generator</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.DomainObjectImpl#getInheritance <em>Inheritance</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.DomainObjectImpl#getAttributes <em>Attributes</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.DomainObjectImpl#getAssociations <em>Associations</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.DomainObjectImpl#getEnumAssociations <em>Enum Associations</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.DomainObjectImpl#getTargetInheritances <em>Target Inheritances</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.DomainObjectImpl#getDiscriminatorColumn <em>Discriminator
 * Column</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.DomainObjectImpl#getDatabaseTable <em>Database Table</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.DomainObjectImpl#getTag <em>Tag</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public class DomainObjectImpl extends JavaTypeImpl implements DomainObject {
	/**
	 * The default value of the '{@link #getLabel() <em>Label</em>}' attribute
	 * @see #getLabel()
	 * @generated
	 * @ordered
	 */
	protected static final String LABEL_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getLabel() <em>Label</em>}' attribute
	 * @see #getLabel()
	 * @generated
	 * @ordered
	 */
	protected String label = LABEL_EDEFAULT;

	/**
	 * The default value of the '{@link #getNamePlural() <em>Name Plural</em>}' attribute
	 * @see #getNamePlural()
	 * @generated
	 * @ordered
	 */
	protected static final String NAME_PLURAL_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getNamePlural() <em>Name Plural</em>}' attribute
	 * @see #getNamePlural()
	 * @generated
	 * @ordered
	 */
	protected String namePlural = NAME_PLURAL_EDEFAULT;

	/**
	 * The default value of the '{@link #getLabelPlural() <em>Label Plural</em>}' attribute
	 * @see #getLabelPlural()
	 * @generated
	 * @ordered
	 */
	protected static final String LABEL_PLURAL_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getLabelPlural() <em>Label Plural</em>}' attribute
	 * @see #getLabelPlural()
	 * @generated
	 * @ordered
	 */
	protected String labelPlural = LABEL_PLURAL_EDEFAULT;

	/**
	 * The default value of the '{@link #getDiscriminatorValue() <em>Discriminator Value</em>}' attribute
	 * @see #getDiscriminatorValue()
	 * @generated
	 * @ordered
	 */
	protected static final String DISCRIMINATOR_VALUE_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getDiscriminatorValue() <em>Discriminator Value</em>}' attribute
	 * @see #getDiscriminatorValue()
	 * @generated
	 * @ordered
	 */
	protected String discriminatorValue = DISCRIMINATOR_VALUE_EDEFAULT;

	/**
	 * The default value of the '{@link #getDiscriminatorColumnType() <em>Discriminator Column Type</em>}' attribute
	 * @see #getDiscriminatorColumnType()
	 * @generated
	 * @ordered
	 */
	protected static final DiscriminatorColumnTypeEnumeration DISCRIMINATOR_COLUMN_TYPE_EDEFAULT = DiscriminatorColumnTypeEnumeration.STRING;

	/**
	 * The cached value of the '{@link #getDiscriminatorColumnType() <em>Discriminator Column Type</em>}' attribute
	 * @see #getDiscriminatorColumnType()
	 * @generated
	 * @ordered
	 */
	protected DiscriminatorColumnTypeEnumeration discriminatorColumnType = DISCRIMINATOR_COLUMN_TYPE_EDEFAULT;

	/**
	 * The default value of the '{@link #getInheritanceType() <em>Inheritance Type</em>}' attribute
	 * @see #getInheritanceType()
	 * @generated
	 * @ordered
	 */
	protected static final InheritanceTypeEnumeration INHERITANCE_TYPE_EDEFAULT = InheritanceTypeEnumeration.NONE;

	/**
	 * The cached value of the '{@link #getInheritanceType() <em>Inheritance Type</em>}' attribute
	 * @see #getInheritanceType()
	 * @generated
	 * @ordered
	 */
	protected InheritanceTypeEnumeration inheritanceType = INHERITANCE_TYPE_EDEFAULT;

	/**
	 * The default value of the '{@link #isPropertyAccess() <em>Property Access</em>}' attribute
	 * @see #isPropertyAccess()
	 * @generated
	 * @ordered
	 */
	protected static final boolean PROPERTY_ACCESS_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isPropertyAccess() <em>Property Access</em>}' attribute
	 * @see #isPropertyAccess()
	 * @generated
	 * @ordered
	 */
	protected boolean propertyAccess = PROPERTY_ACCESS_EDEFAULT;

	/**
	 * The default value of the '{@link #isAbstract() <em>Abstract</em>}' attribute
	 * @see #isAbstract()
	 * @generated
	 * @ordered
	 */
	protected static final boolean ABSTRACT_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isAbstract() <em>Abstract</em>}' attribute
	 * @see #isAbstract()
	 * @generated
	 * @ordered
	 */
	protected boolean abstract_ = ABSTRACT_EDEFAULT;

	/**
	 * The default value of the '{@link #isMappedSuperClass() <em>Mapped Super Class</em>}' attribute
	 * @see #isMappedSuperClass()
	 * @generated
	 * @ordered
	 */
	protected static final boolean MAPPED_SUPER_CLASS_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isMappedSuperClass() <em>Mapped Super Class</em>}' attribute
	 * @see #isMappedSuperClass()
	 * @generated
	 * @ordered
	 */
	protected boolean mappedSuperClass = MAPPED_SUPER_CLASS_EDEFAULT;

	/**
	 * The cached value of the '{@link #getParent() <em>Parent</em>}' reference
	 * @see #getParent()
	 * @generated
	 * @ordered
	 */
	protected DomainObject parent;

	/**
	 * The cached value of the '{@link #getIDGenerator() <em>ID Generator</em>}' containment reference
	 * @see #getIDGenerator()
	 * @generated
	 * @ordered
	 */
	protected IDGenerator iDGenerator;

	/**
	 * The cached value of the '{@link #getInheritance() <em>Inheritance</em>}' containment reference
	 * @see #getInheritance()
	 * @generated
	 * @ordered
	 */
	protected DomainInheritance inheritance;

	/**
	 * The cached value of the '{@link #getAttributes() <em>Attributes</em>}' containment reference list
	 * @see #getAttributes()
	 * @generated
	 * @ordered
	 */
	protected EList<DomainAttribute> attributes;

	/**
	 * The cached value of the '{@link #getAssociations() <em>Associations</em>}' containment reference list
	 * @see #getAssociations()
	 * @generated
	 * @ordered
	 */
	protected EList<AbstractDomainAssociation> associations;

	/**
	 * The cached value of the '{@link #getEnumAssociations() <em>Enum Associations</em>}' containment reference list
	 * @see #getEnumAssociations()
	 * @generated
	 * @ordered
	 */
	protected EList<EnumAssociation> enumAssociations;

	/**
	 * The cached value of the '{@link #getTargetInheritances() <em>Target Inheritances</em>}' reference list
	 * @see #getTargetInheritances()
	 * @generated
	 * @ordered
	 */
	protected EList<DomainInheritance> targetInheritances;

	/**
	 * The cached value of the '{@link #getDiscriminatorColumn() <em>Discriminator Column</em>}' reference
	 * @see #getDiscriminatorColumn()
	 * @generated
	 * @ordered
	 */
	protected DBColumn discriminatorColumn;

	/**
	 * The cached value of the '{@link #getDatabaseTable() <em>Database Table</em>}' containment reference
	 * @see #getDatabaseTable()
	 * @generated
	 * @ordered
	 */
	protected DBTable databaseTable;

	/**
	 * The default value of the '{@link #getTag() <em>Tag</em>}' attribute
	 * @see #getTag()
	 * @generated
	 * @ordered
	 */
	protected static final DomainTagEnumeration TAG_EDEFAULT = DomainTagEnumeration.NONE;

	/**
	 * The cached value of the '{@link #getTag() <em>Tag</em>}' attribute
	 * @see #getTag()
	 * @generated
	 * @ordered
	 */
	protected DomainTagEnumeration tag = TAG_EDEFAULT;

	/**
	 * @generated
	 */
	protected DomainObjectImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.JavaTypeImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return DomainPackage.Literals.DOMAIN_OBJECT;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#getLabel()
	 * @generated
	 */
	@Override
	public String getLabel() {
		return label;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#setLabel(java.lang.String)
	 * @generated
	 */
	@Override
	public void setLabel(String newLabel) {
		final String oldLabel = label;
		label = newLabel;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.DOMAIN_OBJECT__LABEL, oldLabel, label));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#getNamePlural()
	 * @generated
	 */
	@Override
	public String getNamePlural() {
		return namePlural;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#setNamePlural(java.lang.String)
	 * @generated
	 */
	@Override
	public void setNamePlural(String newNamePlural) {
		final String oldNamePlural = namePlural;
		namePlural = newNamePlural;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.DOMAIN_OBJECT__NAME_PLURAL, oldNamePlural, namePlural));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#getLabelPlural()
	 * @generated
	 */
	@Override
	public String getLabelPlural() {
		return labelPlural;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#setLabelPlural(java.lang.String)
	 * @generated
	 */
	@Override
	public void setLabelPlural(String newLabelPlural) {
		final String oldLabelPlural = labelPlural;
		labelPlural = newLabelPlural;

		if (eNotificationRequired())
			eNotify(
					new ENotificationImpl(this, Notification.SET, DomainPackage.DOMAIN_OBJECT__LABEL_PLURAL, oldLabelPlural, labelPlural));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#getDiscriminatorValue()
	 * @generated
	 */
	@Override
	public String getDiscriminatorValue() {
		return discriminatorValue;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#setDiscriminatorValue(java.lang.String)
	 * @generated
	 */
	@Override
	public void setDiscriminatorValue(String newDiscriminatorValue) {
		final String oldDiscriminatorValue = discriminatorValue;
		discriminatorValue = newDiscriminatorValue;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.DOMAIN_OBJECT__DISCRIMINATOR_VALUE,
					oldDiscriminatorValue, discriminatorValue));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#getDiscriminatorColumnType()
	 * @generated
	 */
	@Override
	public DiscriminatorColumnTypeEnumeration getDiscriminatorColumnType() {
		return discriminatorColumnType;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#setDiscriminatorColumnType(net.codecadenza.eclipse.model.domain.
	 * DiscriminatorColumnTypeEnumeration)
	 * @generated
	 */
	@Override
	public void setDiscriminatorColumnType(DiscriminatorColumnTypeEnumeration newDiscriminatorColumnType) {
		final DiscriminatorColumnTypeEnumeration oldDiscriminatorColumnType = discriminatorColumnType;
		discriminatorColumnType = newDiscriminatorColumnType == null ? DISCRIMINATOR_COLUMN_TYPE_EDEFAULT
				: newDiscriminatorColumnType;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.DOMAIN_OBJECT__DISCRIMINATOR_COLUMN_TYPE,
					oldDiscriminatorColumnType, discriminatorColumnType));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#getInheritanceType()
	 * @generated
	 */
	@Override
	public InheritanceTypeEnumeration getInheritanceType() {
		return inheritanceType;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#setInheritanceType(net.codecadenza.eclipse.model.domain.
	 * InheritanceTypeEnumeration)
	 * @generated
	 */
	@Override
	public void setInheritanceType(InheritanceTypeEnumeration newInheritanceType) {
		final InheritanceTypeEnumeration oldInheritanceType = inheritanceType;
		inheritanceType = newInheritanceType == null ? INHERITANCE_TYPE_EDEFAULT : newInheritanceType;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.DOMAIN_OBJECT__INHERITANCE_TYPE, oldInheritanceType,
					inheritanceType));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#isPropertyAccess()
	 * @generated
	 */
	@Override
	public boolean isPropertyAccess() {
		return propertyAccess;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#setPropertyAccess(boolean)
	 * @generated
	 */
	@Override
	public void setPropertyAccess(boolean newPropertyAccess) {
		final boolean oldPropertyAccess = propertyAccess;
		propertyAccess = newPropertyAccess;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.DOMAIN_OBJECT__PROPERTY_ACCESS, oldPropertyAccess,
					propertyAccess));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#isAbstract()
	 * @generated
	 */
	@Override
	public boolean isAbstract() {
		return abstract_;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#setAbstract(boolean)
	 * @generated
	 */
	@Override
	public void setAbstract(boolean newAbstract) {
		final boolean oldAbstract = abstract_;
		abstract_ = newAbstract;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.DOMAIN_OBJECT__ABSTRACT, oldAbstract, abstract_));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#isMappedSuperClass()
	 * @generated
	 */
	@Override
	public boolean isMappedSuperClass() {
		return mappedSuperClass;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#setMappedSuperClass(boolean)
	 * @generated
	 */
	@Override
	public void setMappedSuperClass(boolean newMappedSuperClass) {
		final boolean oldMappedSuperClass = mappedSuperClass;
		mappedSuperClass = newMappedSuperClass;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.DOMAIN_OBJECT__MAPPED_SUPER_CLASS, oldMappedSuperClass,
					mappedSuperClass));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#getParent()
	 * @generated
	 */
	@Override
	public DomainObject getParent() {
		if (parent != null && parent.eIsProxy()) {
			final var oldParent = (InternalEObject) parent;
			parent = (DomainObject) eResolveProxy(oldParent);

			if (parent != oldParent && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, DomainPackage.DOMAIN_OBJECT__PARENT, oldParent, parent));
		}

		return parent;
	}

	/**
	 * @return the cached object instance
	 * @generated
	 */
	public DomainObject basicGetParent() {
		return parent;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#setParent(net.codecadenza.eclipse.model.domain.DomainObject)
	 * @generated
	 */
	@Override
	public void setParent(DomainObject newParent) {
		final DomainObject oldParent = parent;
		parent = newParent;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.DOMAIN_OBJECT__PARENT, oldParent, parent));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#getIDGenerator()
	 * @generated
	 */
	@Override
	public IDGenerator getIDGenerator() {
		return iDGenerator;
	}

	/**
	 * @param newIDGenerator
	 * @param msgs
	 * @return a chain of notifications
	 * @generated
	 */
	public NotificationChain basicSetIDGenerator(IDGenerator newIDGenerator, NotificationChain msgs) {
		final IDGenerator oldIDGenerator = iDGenerator;
		iDGenerator = newIDGenerator;

		if (eNotificationRequired()) {
			final var notification = new ENotificationImpl(this, Notification.SET, DomainPackage.DOMAIN_OBJECT__ID_GENERATOR,
					oldIDGenerator, newIDGenerator);

			if (msgs == null)
				msgs = notification;
			else
				msgs.add(notification);
		}

		return msgs;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#setIDGenerator(net.codecadenza.eclipse.model.domain.IDGenerator)
	 * @generated
	 */
	@Override
	public void setIDGenerator(IDGenerator newIDGenerator) {
		if (newIDGenerator != iDGenerator) {
			NotificationChain msgs = null;

			if (iDGenerator != null)
				msgs = ((InternalEObject) iDGenerator).eInverseRemove(this,
						EOPPOSITE_FEATURE_BASE - DomainPackage.DOMAIN_OBJECT__ID_GENERATOR, null, msgs);

			if (newIDGenerator != null)
				msgs = ((InternalEObject) newIDGenerator).eInverseAdd(this,
						EOPPOSITE_FEATURE_BASE - DomainPackage.DOMAIN_OBJECT__ID_GENERATOR, null, msgs);

			msgs = basicSetIDGenerator(newIDGenerator, msgs);

			if (msgs != null)
				msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.DOMAIN_OBJECT__ID_GENERATOR, newIDGenerator,
					newIDGenerator));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#getInheritance()
	 * @generated
	 */
	@Override
	public DomainInheritance getInheritance() {
		return inheritance;
	}

	/**
	 * @param newInheritance
	 * @param msgs
	 * @return a chain of notifications
	 * @generated
	 */
	public NotificationChain basicSetInheritance(DomainInheritance newInheritance, NotificationChain msgs) {
		final DomainInheritance oldInheritance = inheritance;
		inheritance = newInheritance;

		if (eNotificationRequired()) {
			final var notification = new ENotificationImpl(this, Notification.SET, DomainPackage.DOMAIN_OBJECT__INHERITANCE,
					oldInheritance, newInheritance);

			if (msgs == null)
				msgs = notification;
			else
				msgs.add(notification);
		}

		return msgs;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#setInheritance(net.codecadenza.eclipse.model.domain.DomainInheritance)
	 * @generated
	 */
	@Override
	public void setInheritance(DomainInheritance newInheritance) {
		if (newInheritance != inheritance) {
			NotificationChain msgs = null;

			if (inheritance != null)
				msgs = ((InternalEObject) inheritance).eInverseRemove(this, DomainPackage.DOMAIN_INHERITANCE__SOURCE,
						DomainInheritance.class, msgs);

			if (newInheritance != null)
				msgs = ((InternalEObject) newInheritance).eInverseAdd(this, DomainPackage.DOMAIN_INHERITANCE__SOURCE,
						DomainInheritance.class, msgs);

			msgs = basicSetInheritance(newInheritance, msgs);

			if (msgs != null)
				msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.DOMAIN_OBJECT__INHERITANCE, newInheritance,
					newInheritance));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#getAttributes()
	 * @generated
	 */
	@Override
	public EList<DomainAttribute> getAttributes() {
		if (attributes == null)
			attributes = new EObjectContainmentWithInverseEList<>(DomainAttribute.class, this, DomainPackage.DOMAIN_OBJECT__ATTRIBUTES,
					DomainPackage.DOMAIN_ATTRIBUTE__DOMAIN_OBJECT);

		return attributes;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#getAssociations()
	 * @generated
	 */
	@Override
	public EList<AbstractDomainAssociation> getAssociations() {
		if (associations == null)
			associations = new EObjectContainmentWithInverseEList<>(AbstractDomainAssociation.class, this,
					DomainPackage.DOMAIN_OBJECT__ASSOCIATIONS, DomainPackage.ABSTRACT_DOMAIN_ASSOCIATION__DOMAIN_OBJECT);

		return associations;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#getEnumAssociations()
	 * @generated
	 */
	@Override
	public EList<EnumAssociation> getEnumAssociations() {
		if (enumAssociations == null)
			enumAssociations = new EObjectContainmentWithInverseEList<>(EnumAssociation.class, this,
					DomainPackage.DOMAIN_OBJECT__ENUM_ASSOCIATIONS, DomainPackage.ENUM_ASSOCIATION__SOURCE);

		return enumAssociations;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#getTargetInheritances()
	 * @generated
	 */
	@Override
	public EList<DomainInheritance> getTargetInheritances() {
		if (targetInheritances == null)
			targetInheritances = new EObjectResolvingEList<>(DomainInheritance.class, this,
					DomainPackage.DOMAIN_OBJECT__TARGET_INHERITANCES);

		return targetInheritances;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#getDiscriminatorColumn()
	 * @generated
	 */
	@Override
	public DBColumn getDiscriminatorColumn() {
		if (discriminatorColumn != null && discriminatorColumn.eIsProxy()) {
			final var oldDiscriminatorColumn = (InternalEObject) discriminatorColumn;
			discriminatorColumn = (DBColumn) eResolveProxy(oldDiscriminatorColumn);

			if (discriminatorColumn != oldDiscriminatorColumn && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, DomainPackage.DOMAIN_OBJECT__DISCRIMINATOR_COLUMN,
						oldDiscriminatorColumn, discriminatorColumn));
		}

		return discriminatorColumn;
	}

	/**
	 * @return the cached object instance
	 * @generated
	 */
	public DBColumn basicGetDiscriminatorColumn() {
		return discriminatorColumn;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#setDiscriminatorColumn(net.codecadenza.eclipse.model.db.DBColumn)
	 * @generated
	 */
	@Override
	public void setDiscriminatorColumn(DBColumn newDiscriminatorColumn) {
		final DBColumn oldDiscriminatorColumn = discriminatorColumn;
		discriminatorColumn = newDiscriminatorColumn;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.DOMAIN_OBJECT__DISCRIMINATOR_COLUMN,
					oldDiscriminatorColumn, discriminatorColumn));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#getDatabaseTable()
	 * @generated
	 */
	@Override
	public DBTable getDatabaseTable() {
		return databaseTable;
	}

	/**
	 * @param newDatabaseTable
	 * @param msgs
	 * @return a chain of notifications
	 * @generated
	 */
	public NotificationChain basicSetDatabaseTable(DBTable newDatabaseTable, NotificationChain msgs) {
		final DBTable oldDatabaseTable = databaseTable;
		databaseTable = newDatabaseTable;

		if (eNotificationRequired()) {
			final var notification = new ENotificationImpl(this, Notification.SET, DomainPackage.DOMAIN_OBJECT__DATABASE_TABLE,
					oldDatabaseTable, newDatabaseTable);

			if (msgs == null)
				msgs = notification;
			else
				msgs.add(notification);
		}

		return msgs;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#setDatabaseTable(net.codecadenza.eclipse.model.db.DBTable)
	 * @generated
	 */
	@Override
	public void setDatabaseTable(DBTable newDatabaseTable) {
		if (newDatabaseTable != databaseTable) {
			NotificationChain msgs = null;

			if (databaseTable != null)
				msgs = ((InternalEObject) databaseTable).eInverseRemove(this,
						EOPPOSITE_FEATURE_BASE - DomainPackage.DOMAIN_OBJECT__DATABASE_TABLE, null, msgs);

			if (newDatabaseTable != null)
				msgs = ((InternalEObject) newDatabaseTable).eInverseAdd(this,
						EOPPOSITE_FEATURE_BASE - DomainPackage.DOMAIN_OBJECT__DATABASE_TABLE, null, msgs);

			msgs = basicSetDatabaseTable(newDatabaseTable, msgs);

			if (msgs != null)
				msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.DOMAIN_OBJECT__DATABASE_TABLE, newDatabaseTable,
					newDatabaseTable));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#getTag()
	 * @generated
	 */
	@Override
	public DomainTagEnumeration getTag() {
		return tag;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#setTag(net.codecadenza.eclipse.model.domain.DomainTagEnumeration)
	 * @generated
	 */
	@Override
	public void setTag(DomainTagEnumeration newTag) {
		final DomainTagEnumeration oldTag = tag;
		tag = newTag == null ? TAG_EDEFAULT : newTag;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.DOMAIN_OBJECT__TAG, oldTag, tag));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.JavaTypeImpl#eInverseAdd(org.eclipse.emf.ecore.InternalEObject, int,
	 * org.eclipse.emf.common.notify.NotificationChain)
	 * @generated
	 */
	@SuppressWarnings("unchecked")
	@Override
	public NotificationChain eInverseAdd(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case DomainPackage.DOMAIN_OBJECT__INHERITANCE:
				if (inheritance != null)
					msgs = ((InternalEObject) inheritance).eInverseRemove(this,
							EOPPOSITE_FEATURE_BASE - DomainPackage.DOMAIN_OBJECT__INHERITANCE, null, msgs);

				return basicSetInheritance((DomainInheritance) otherEnd, msgs);
			case DomainPackage.DOMAIN_OBJECT__ATTRIBUTES:
				return ((InternalEList<InternalEObject>) (InternalEList<?>) getAttributes()).basicAdd(otherEnd, msgs);
			case DomainPackage.DOMAIN_OBJECT__ASSOCIATIONS:
				return ((InternalEList<InternalEObject>) (InternalEList<?>) getAssociations()).basicAdd(otherEnd, msgs);
			case DomainPackage.DOMAIN_OBJECT__ENUM_ASSOCIATIONS:
				return ((InternalEList<InternalEObject>) (InternalEList<?>) getEnumAssociations()).basicAdd(otherEnd, msgs);
		}

		return super.eInverseAdd(otherEnd, featureID, msgs);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.JavaTypeImpl#eInverseRemove(org.eclipse.emf.ecore.InternalEObject, int,
	 * org.eclipse.emf.common.notify.NotificationChain)
	 * @generated
	 */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case DomainPackage.DOMAIN_OBJECT__ID_GENERATOR:
				return basicSetIDGenerator(null, msgs);
			case DomainPackage.DOMAIN_OBJECT__INHERITANCE:
				return basicSetInheritance(null, msgs);
			case DomainPackage.DOMAIN_OBJECT__ATTRIBUTES:
				return ((InternalEList<?>) getAttributes()).basicRemove(otherEnd, msgs);
			case DomainPackage.DOMAIN_OBJECT__ASSOCIATIONS:
				return ((InternalEList<?>) getAssociations()).basicRemove(otherEnd, msgs);
			case DomainPackage.DOMAIN_OBJECT__ENUM_ASSOCIATIONS:
				return ((InternalEList<?>) getEnumAssociations()).basicRemove(otherEnd, msgs);
			case DomainPackage.DOMAIN_OBJECT__DATABASE_TABLE:
				return basicSetDatabaseTable(null, msgs);
		}

		return super.eInverseRemove(otherEnd, featureID, msgs);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.JavaTypeImpl#eGet(int, boolean, boolean)
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case DomainPackage.DOMAIN_OBJECT__LABEL:
				return getLabel();
			case DomainPackage.DOMAIN_OBJECT__NAME_PLURAL:
				return getNamePlural();
			case DomainPackage.DOMAIN_OBJECT__LABEL_PLURAL:
				return getLabelPlural();
			case DomainPackage.DOMAIN_OBJECT__DISCRIMINATOR_VALUE:
				return getDiscriminatorValue();
			case DomainPackage.DOMAIN_OBJECT__DISCRIMINATOR_COLUMN_TYPE:
				return getDiscriminatorColumnType();
			case DomainPackage.DOMAIN_OBJECT__INHERITANCE_TYPE:
				return getInheritanceType();
			case DomainPackage.DOMAIN_OBJECT__PROPERTY_ACCESS:
				return isPropertyAccess();
			case DomainPackage.DOMAIN_OBJECT__ABSTRACT:
				return isAbstract();
			case DomainPackage.DOMAIN_OBJECT__MAPPED_SUPER_CLASS:
				return isMappedSuperClass();
			case DomainPackage.DOMAIN_OBJECT__PARENT:
				if (resolve)
					return getParent();

				return basicGetParent();
			case DomainPackage.DOMAIN_OBJECT__ID_GENERATOR:
				return getIDGenerator();
			case DomainPackage.DOMAIN_OBJECT__INHERITANCE:
				return getInheritance();
			case DomainPackage.DOMAIN_OBJECT__ATTRIBUTES:
				return getAttributes();
			case DomainPackage.DOMAIN_OBJECT__ASSOCIATIONS:
				return getAssociations();
			case DomainPackage.DOMAIN_OBJECT__ENUM_ASSOCIATIONS:
				return getEnumAssociations();
			case DomainPackage.DOMAIN_OBJECT__TARGET_INHERITANCES:
				return getTargetInheritances();
			case DomainPackage.DOMAIN_OBJECT__DISCRIMINATOR_COLUMN:
				if (resolve)
					return getDiscriminatorColumn();

				return basicGetDiscriminatorColumn();
			case DomainPackage.DOMAIN_OBJECT__DATABASE_TABLE:
				return getDatabaseTable();
			case DomainPackage.DOMAIN_OBJECT__TAG:
				return getTag();
		}

		return super.eGet(featureID, resolve, coreType);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.JavaTypeImpl#eSet(int, java.lang.Object)
	 * @generated
	 */
	@SuppressWarnings("unchecked")
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case DomainPackage.DOMAIN_OBJECT__LABEL:
				setLabel((String) newValue);
				return;
			case DomainPackage.DOMAIN_OBJECT__NAME_PLURAL:
				setNamePlural((String) newValue);
				return;
			case DomainPackage.DOMAIN_OBJECT__LABEL_PLURAL:
				setLabelPlural((String) newValue);
				return;
			case DomainPackage.DOMAIN_OBJECT__DISCRIMINATOR_VALUE:
				setDiscriminatorValue((String) newValue);
				return;
			case DomainPackage.DOMAIN_OBJECT__DISCRIMINATOR_COLUMN_TYPE:
				setDiscriminatorColumnType((DiscriminatorColumnTypeEnumeration) newValue);
				return;
			case DomainPackage.DOMAIN_OBJECT__INHERITANCE_TYPE:
				setInheritanceType((InheritanceTypeEnumeration) newValue);
				return;
			case DomainPackage.DOMAIN_OBJECT__PROPERTY_ACCESS:
				setPropertyAccess((Boolean) newValue);
				return;
			case DomainPackage.DOMAIN_OBJECT__ABSTRACT:
				setAbstract((Boolean) newValue);
				return;
			case DomainPackage.DOMAIN_OBJECT__MAPPED_SUPER_CLASS:
				setMappedSuperClass((Boolean) newValue);
				return;
			case DomainPackage.DOMAIN_OBJECT__PARENT:
				setParent((DomainObject) newValue);
				return;
			case DomainPackage.DOMAIN_OBJECT__ID_GENERATOR:
				setIDGenerator((IDGenerator) newValue);
				return;
			case DomainPackage.DOMAIN_OBJECT__INHERITANCE:
				setInheritance((DomainInheritance) newValue);
				return;
			case DomainPackage.DOMAIN_OBJECT__ATTRIBUTES:
				getAttributes().clear();
				getAttributes().addAll((Collection<? extends DomainAttribute>) newValue);
				return;
			case DomainPackage.DOMAIN_OBJECT__ASSOCIATIONS:
				getAssociations().clear();
				getAssociations().addAll((Collection<? extends AbstractDomainAssociation>) newValue);
				return;
			case DomainPackage.DOMAIN_OBJECT__ENUM_ASSOCIATIONS:
				getEnumAssociations().clear();
				getEnumAssociations().addAll((Collection<? extends EnumAssociation>) newValue);
				return;
			case DomainPackage.DOMAIN_OBJECT__TARGET_INHERITANCES:
				getTargetInheritances().clear();
				getTargetInheritances().addAll((Collection<? extends DomainInheritance>) newValue);
				return;
			case DomainPackage.DOMAIN_OBJECT__DISCRIMINATOR_COLUMN:
				setDiscriminatorColumn((DBColumn) newValue);
				return;
			case DomainPackage.DOMAIN_OBJECT__DATABASE_TABLE:
				setDatabaseTable((DBTable) newValue);
				return;
			case DomainPackage.DOMAIN_OBJECT__TAG:
				setTag((DomainTagEnumeration) newValue);
				return;
		}

		super.eSet(featureID, newValue);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.JavaTypeImpl#eUnset(int)
	 * @generated
	 */
	@Override
	public void eUnset(int featureID) {
		switch (featureID) {
			case DomainPackage.DOMAIN_OBJECT__LABEL:
				setLabel(LABEL_EDEFAULT);
				return;
			case DomainPackage.DOMAIN_OBJECT__NAME_PLURAL:
				setNamePlural(NAME_PLURAL_EDEFAULT);
				return;
			case DomainPackage.DOMAIN_OBJECT__LABEL_PLURAL:
				setLabelPlural(LABEL_PLURAL_EDEFAULT);
				return;
			case DomainPackage.DOMAIN_OBJECT__DISCRIMINATOR_VALUE:
				setDiscriminatorValue(DISCRIMINATOR_VALUE_EDEFAULT);
				return;
			case DomainPackage.DOMAIN_OBJECT__DISCRIMINATOR_COLUMN_TYPE:
				setDiscriminatorColumnType(DISCRIMINATOR_COLUMN_TYPE_EDEFAULT);
				return;
			case DomainPackage.DOMAIN_OBJECT__INHERITANCE_TYPE:
				setInheritanceType(INHERITANCE_TYPE_EDEFAULT);
				return;
			case DomainPackage.DOMAIN_OBJECT__PROPERTY_ACCESS:
				setPropertyAccess(PROPERTY_ACCESS_EDEFAULT);
				return;
			case DomainPackage.DOMAIN_OBJECT__ABSTRACT:
				setAbstract(ABSTRACT_EDEFAULT);
				return;
			case DomainPackage.DOMAIN_OBJECT__MAPPED_SUPER_CLASS:
				setMappedSuperClass(MAPPED_SUPER_CLASS_EDEFAULT);
				return;
			case DomainPackage.DOMAIN_OBJECT__PARENT:
				setParent((DomainObject) null);
				return;
			case DomainPackage.DOMAIN_OBJECT__ID_GENERATOR:
				setIDGenerator((IDGenerator) null);
				return;
			case DomainPackage.DOMAIN_OBJECT__INHERITANCE:
				setInheritance((DomainInheritance) null);
				return;
			case DomainPackage.DOMAIN_OBJECT__ATTRIBUTES:
				getAttributes().clear();
				return;
			case DomainPackage.DOMAIN_OBJECT__ASSOCIATIONS:
				getAssociations().clear();
				return;
			case DomainPackage.DOMAIN_OBJECT__ENUM_ASSOCIATIONS:
				getEnumAssociations().clear();
				return;
			case DomainPackage.DOMAIN_OBJECT__TARGET_INHERITANCES:
				getTargetInheritances().clear();
				return;
			case DomainPackage.DOMAIN_OBJECT__DISCRIMINATOR_COLUMN:
				setDiscriminatorColumn((DBColumn) null);
				return;
			case DomainPackage.DOMAIN_OBJECT__DATABASE_TABLE:
				setDatabaseTable((DBTable) null);
				return;
			case DomainPackage.DOMAIN_OBJECT__TAG:
				setTag(TAG_EDEFAULT);
				return;
		}

		super.eUnset(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.JavaTypeImpl#eIsSet(int)
	 * @generated
	 */
	@Override
	public boolean eIsSet(int featureID) {
		switch (featureID) {
			case DomainPackage.DOMAIN_OBJECT__LABEL:
				return label != null;
			case DomainPackage.DOMAIN_OBJECT__NAME_PLURAL:
				return namePlural != null;
			case DomainPackage.DOMAIN_OBJECT__LABEL_PLURAL:
				return labelPlural != null;
			case DomainPackage.DOMAIN_OBJECT__DISCRIMINATOR_VALUE:
				return discriminatorValue != null;
			case DomainPackage.DOMAIN_OBJECT__DISCRIMINATOR_COLUMN_TYPE:
				return discriminatorColumnType != DISCRIMINATOR_COLUMN_TYPE_EDEFAULT;
			case DomainPackage.DOMAIN_OBJECT__INHERITANCE_TYPE:
				return inheritanceType != INHERITANCE_TYPE_EDEFAULT;
			case DomainPackage.DOMAIN_OBJECT__PROPERTY_ACCESS:
				return propertyAccess != PROPERTY_ACCESS_EDEFAULT;
			case DomainPackage.DOMAIN_OBJECT__ABSTRACT:
				return abstract_ != ABSTRACT_EDEFAULT;
			case DomainPackage.DOMAIN_OBJECT__MAPPED_SUPER_CLASS:
				return mappedSuperClass != MAPPED_SUPER_CLASS_EDEFAULT;
			case DomainPackage.DOMAIN_OBJECT__PARENT:
				return parent != null;
			case DomainPackage.DOMAIN_OBJECT__ID_GENERATOR:
				return iDGenerator != null;
			case DomainPackage.DOMAIN_OBJECT__INHERITANCE:
				return inheritance != null;
			case DomainPackage.DOMAIN_OBJECT__ATTRIBUTES:
				return attributes != null && !attributes.isEmpty();
			case DomainPackage.DOMAIN_OBJECT__ASSOCIATIONS:
				return associations != null && !associations.isEmpty();
			case DomainPackage.DOMAIN_OBJECT__ENUM_ASSOCIATIONS:
				return enumAssociations != null && !enumAssociations.isEmpty();
			case DomainPackage.DOMAIN_OBJECT__TARGET_INHERITANCES:
				return targetInheritances != null && !targetInheritances.isEmpty();
			case DomainPackage.DOMAIN_OBJECT__DISCRIMINATOR_COLUMN:
				return discriminatorColumn != null;
			case DomainPackage.DOMAIN_OBJECT__DATABASE_TABLE:
				return databaseTable != null;
			case DomainPackage.DOMAIN_OBJECT__TAG:
				return tag != TAG_EDEFAULT;
		}

		return super.eIsSet(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.JavaTypeImpl#toString()
	 * @generated
	 */
	@Override
	public String toString() {
		if (eIsProxy())
			return super.toString();

		final var result = new StringBuilder(super.toString());
		result.append(" (label: ");
		result.append(label);
		result.append(", namePlural: ");
		result.append(namePlural);
		result.append(", labelPlural: ");
		result.append(labelPlural);
		result.append(", discriminatorValue: ");
		result.append(discriminatorValue);
		result.append(", discriminatorColumnType: ");
		result.append(discriminatorColumnType);
		result.append(", inheritanceType: ");
		result.append(inheritanceType);
		result.append(", propertyAccess: ");
		result.append(propertyAccess);
		result.append(", abstract: ");
		result.append(abstract_);
		result.append(", mappedSuperClass: ");
		result.append(mappedSuperClass);
		result.append(", tag: ");
		result.append(tag);
		result.append(')');

		return result.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#getFullInheritanceTree()
	 * @generated not
	 */
	@Override
	public Collection<DomainObject> getFullInheritanceTree() {
		final var inheritanceTree = new BasicEList<DomainObject>();
		DomainObject bean = this;

		// Get full inheritance tree of domain object
		while (true) {
			inheritanceTree.add(bean);

			final DomainObject parentDomainObj = bean.getParent();

			if (parentDomainObj == null)
				break;

			bean = parentDomainObj;
		}

		return inheritanceTree;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#getValidAssociationTags(net.codecadenza.eclipse.model.domain.
	 * DomainObject)
	 * @generated not
	 */
	@Override
	public ArrayList<String> getValidAssociationTags(DomainObject target) {
		final EnumSet<AssociationTagEnumeration> existingTags = EnumSet.noneOf(AssociationTagEnumeration.class);
		final var list = new ArrayList<String>();
		list.add(AssociationTagEnumeration.NONE.getName());

		// First we save all tags that are already in use
		for (final AbstractDomainAssociation assoc : getAssociations())
			if (assoc.getTag() != AssociationTagEnumeration.NONE)
				existingTags.add(assoc.getTag());

		// If the association refers to the domain object that represents a client we may add an appropriate tag.
		if (target != null && target.getTag() == DomainTagEnumeration.CLIENT
				&& !existingTags.contains(AssociationTagEnumeration.CLIENT_REFERENCE))
			list.add(AssociationTagEnumeration.CLIENT_REFERENCE.name());

		if (getTag() == DomainTagEnumeration.NONE)
			return list;

		for (final AssociationTagEnumeration item : AssociationTagEnumeration.values())
			if (item.getName().startsWith(getTag().getName()) && !existingTags.contains(item))
				list.add(item.getName());

		return list;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#getValidAttributeTags()
	 * @generated not
	 */
	@Override
	public ArrayList<String> getValidAttributeTags() {
		final EnumSet<AttributeTagEnumeration> existingTags = EnumSet.noneOf(AttributeTagEnumeration.class);
		final var list = new ArrayList<String>();
		list.add(AttributeTagEnumeration.NONE.getName());

		for (final DomainAttribute attr : getAttributes())
			if (attr.getTag() != AttributeTagEnumeration.NONE) {
				existingTags.add(attr.getTag());

				// It shouldn't be allowed to define two content specific fields for one domain object!
				if (attr.getTag() == AttributeTagEnumeration.DOCUMENT_DATA)
					existingTags.add(AttributeTagEnumeration.DOCUMENT_REF);

				if (attr.getTag() == AttributeTagEnumeration.DOCUMENT_REF)
					existingTags.add(AttributeTagEnumeration.DOCUMENT_DATA);
			}

		// Validate if special mandating tag may be added
		if (isMandated() && getDisplayAttribute() == null && !existingTags.contains(AttributeTagEnumeration.CLIENT_DISPLAY))
			list.add(AttributeTagEnumeration.CLIENT_DISPLAY.name());

		if (getTag() == DomainTagEnumeration.NONE)
			return list;

		// Just add attribute tags that belong to domain object tag. Check via name!
		for (final AttributeTagEnumeration item : AttributeTagEnumeration.values())
			if (item.getName().startsWith(getTag().getName()) && !existingTags.contains(item))
				list.add(item.getName());

		return list;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#getPKAttribute()
	 * @generated not
	 */
	@Override
	public DomainAttribute getPKAttribute() {
		return getAllAttributes().stream().filter(DomainAttribute::isPk).findFirst().orElse(null);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#getDisplayAttribute()
	 * @generated not
	 */
	@Override
	public DomainAttribute getDisplayAttribute() {
		return getAllAttributes().stream().filter(DomainAttribute::isDisplayAttribute).findFirst().orElse(null);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#getAllValidDTOAttributes(java.util.HashSet)
	 * @generated not
	 */
	@Override
	public HashSet<DomainAttribute> getAllValidDTOAttributes(HashSet<AbstractDomainAssociation> assocSet) {
		final var attributeSet = new HashSet<DomainAttribute>();

		attributeSet.addAll(getAllAttributes());

		for (final AbstractDomainAssociation assoc : getAllAssociations()) {
			if (assoc.getTarget().equals(this))
				continue;

			// Check if this association has been already analyzed in order to avoid an infinite loop!
			if (assocSet.contains(assoc))
				continue;

			assocSet.add(assoc);

			if (assoc instanceof OneToOneAssociation || assoc instanceof ManyToOneAssociation)
				attributeSet.addAll(assoc.getTarget().getAllValidDTOAttributes(assocSet));
		}

		return attributeSet;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#getAllValidDTOAttributes()
	 * @generated not
	 */
	@Override
	public HashSet<DomainAttribute> getAllValidDTOAttributes() {
		return getAllValidDTOAttributes(new HashSet<>());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#getRootParentDomainObject(boolean)
	 * @generated not
	 */
	@Override
	public DomainObject getRootParentDomainObject(boolean includeMappedSuperClass) {
		DomainObject b = this;

		while (true) {
			final DomainObject parentDomainObj = b.getParent();

			if (parentDomainObj != null) {
				if (parentDomainObj.isMappedSuperClass()) {
					if (includeMappedSuperClass)
						return parentDomainObj;

					return b;
				}
			}
			else
				return b;

			b = parentDomainObj;
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#getAllLobAttributes()
	 * @generated not
	 */
	@Override
	public BasicEList<DomainAttribute> getAllLobAttributes() {
		final var attrList = new BasicEList<DomainAttribute>();

		// Get all LOB attributes of this domain object
		for (final DomainAttribute a : getAllAttributes())
			if (a.isLob() || a.getTag() == AttributeTagEnumeration.DOCUMENT_REF)
				attrList.add(a);

		// Get all LOB attributes of any one-to-one association
		for (final AbstractDomainAssociation assoc : getAllAssociations())
			if (assoc instanceof OneToOneAssociation) {
				final DomainObject refObject = assoc.getTarget();

				for (final DomainAttribute a : refObject.getAllAttributes())
					if (a.isLob() || a.getTag() == AttributeTagEnumeration.DOCUMENT_REF)
						attrList.add(a);
			}

		return attrList;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#getAllAssociations()
	 * @generated not
	 */
	@Override
	public BasicEList<AbstractDomainAssociation> getAllAssociations() {
		final var assocList = new BasicEList<AbstractDomainAssociation>();

		getFullInheritanceTree().forEach(o -> assocList.addAll(o.getAssociations()));

		return assocList;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#getAllAttributes()
	 * @generated not
	 */
	@Override
	public BasicEList<DomainAttribute> getAllAttributes() {
		final var attrList = new BasicEList<DomainAttribute>();

		getFullInheritanceTree().forEach(o -> attrList.addAll(o.getAttributes()));

		return attrList;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#isMandated(java.util.HashSet)
	 * @generated not
	 */
	@Override
	public boolean isMandated(HashSet<AbstractDomainAssociation> assocSet) {
		for (final AbstractDomainAssociation assoc : getAllAssociations()) {
			if (assoc.getTag() == AssociationTagEnumeration.CLIENT_REFERENCE)
				return true;

			if (assoc.getTarget().equals(this))
				continue;

			// Check if this association has been already analyzed in order to avoid an infinite loop!
			if (assocSet.contains(assoc))
				continue;

			assocSet.add(assoc);

			final DomainObject refDomainObj = assoc.getTarget();

			if ((assoc instanceof final ManyToOneAssociation mto && !mto.isOptional() && refDomainObj.isMandated(assocSet))
					|| (assoc instanceof final OneToOneAssociation oto && !oto.isOptional() && refDomainObj.isMandated(assocSet)))
				return true;
		}

		return false;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#isMandated()
	 * @generated not
	 */
	@Override
	public boolean isMandated() {
		if (!this.getNamespace().getProject().isMandatingSupported())
			return false;

		return isMandated(new HashSet<>());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#hasUserReference(java.util.HashSet)
	 * @generated not
	 */
	@Override
	public boolean hasUserReference(HashSet<AbstractDomainAssociation> assocSet) {
		final DomainObject objUser = getNamespace().getProject().getDomainObjectByTag(DomainTagEnumeration.USER);

		// Now we check the associations of the referenced domain objects
		for (final AbstractDomainAssociation assoc : getAllAssociations()) {
			if (assoc.getTarget().equals(this))
				continue;

			// Check if this association has been already analyzed in order to avoid an infinite loop!
			if (assocSet.contains(assoc))
				continue;

			assocSet.add(assoc);

			if ((assoc instanceof ManyToOneAssociation && assoc.getTarget().equals(objUser))
					|| (assoc instanceof final OneToOneAssociation oto && !oto.isOptional() && oto.getTarget().hasUserReference(assocSet)))
				return true;
		}

		return false;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#hasUserReference()
	 * @generated not
	 */
	@Override
	public boolean hasUserReference() {
		final DTOBean logOnDTO = this.getNamespace().getProject().getApplicationLogOnDTO();

		if (logOnDTO == null)
			return false;

		return hasUserReference(new HashSet<>());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#findRepositoryMethod(net.codecadenza.eclipse.model.repository.
	 * RepositoryMethodTypeEnumeration, net.codecadenza.eclipse.model.domain.DomainAttribute)
	 * @generated not
	 */
	@Override
	public RepositoryMethod findRepositoryMethod(RepositoryMethodTypeEnumeration type, DomainAttribute attribute) {
		final Repository repository = getNamespace().getProject().getAllRepositoriesOfProject().stream()
				.filter(c -> c.getDomainObject().equals(this)).findFirst().orElse(null);

		if (repository == null)
			return null;

		for (final RepositoryMethod m : repository.getRepositoryMethods())
			if (m.getMethodType() == type) {
				if (attribute == null)
					return m;

				for (final MethodParameter param : m.getMethodParameters()) {
					if (!(param instanceof final RepositoryMethodParameter repositoryParam))
						continue;

					if (repositoryParam.getAttribute() != null && repositoryParam.getAttribute().equals(attribute))
						return m;
				}
			}

		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#findRepositoryMethod(net.codecadenza.eclipse.model.repository.
	 * RepositoryMethodTypeEnumeration)
	 * @generated not
	 */
	@Override
	public RepositoryMethod findRepositoryMethod(RepositoryMethodTypeEnumeration type) {
		return findRepositoryMethod(type, null);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#isDTOSharingAllowed()
	 * @generated not
	 */
	@Override
	public boolean isDTOSharingAllowed() {
		// Collect all attributes of all one-to-one associations
		final List<DomainAttribute> otoAttributes = getAllAssociations().stream().filter(OneToOneAssociation.class::isInstance)
				.map(AbstractDomainAssociation::getTarget).map(DomainObject::getAllAttributes).flatMap(List::stream).toList();

		final var allAttributes = new ArrayList<>(getAllAttributes());
		allAttributes.addAll(otoAttributes);

		return allAttributes.stream().filter(DomainAttribute::isLob).findFirst().map(_ -> false).orElse(true);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#getSharedDTO()
	 * @generated not
	 */
	@Override
	public DTOBean getSharedDTO() {
		for (final Form existingForm : getNamespace().getProject().getAllFormsOfProject()) {
			if (!existingForm.getDomainObject().equals(this))
				continue;

			if (existingForm.getFormType() == FormTypeEnumeration.LOV || existingForm.getFormType() == FormTypeEnumeration.SIMPLE_VIEW)
				continue;

			if (existingForm.getFormType() == FormTypeEnumeration.SEARCHABLE_VIEW)
				continue;

			if (existingForm.getDTO().isShared())
				return existingForm.getDTO();
		}

		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#getSourceFile()
	 * @generated not
	 */
	@Override
	public JavaFile getSourceFile() {
		final var javaFile = new JavaFile(getNamespace(), BuildArtifactType.DOMAIN, name, getNamespace().toString());
		javaFile.setComment(comment);

		return javaFile;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#getListenerSourceFile()
	 * @generated not
	 */
	@Override
	public JavaFile getListenerSourceFile() {
		final String listenerPackage = getNamespace().toString() + SUB_PACKAGE_LISTENER;
		final String className = name + LISTENER_SUFFIX;

		final var javaFile = new JavaFile(getNamespace(), BuildArtifactType.DOMAIN, className, listenerPackage);
		javaFile.setComment("Callback listener for " + label + " objects");

		return javaFile;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#getMetaModelSourceFile()
	 * @generated not
	 */
	@Override
	public JavaFile getMetaModelSourceFile() {
		final var javaFile = new JavaFile(getNamespace(), BuildArtifactType.DOMAIN, name + "_", getNamespace().toString());
		javaFile.setComment("Canonical meta-model for domain object {@link " + name + "}");

		return javaFile;
	}

}
