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

import static net.codecadenza.eclipse.shared.Constants.PACK_JAVA_TIME;

import java.util.Optional;
import net.codecadenza.eclipse.model.db.DBColumn;
import net.codecadenza.eclipse.model.db.DBTable;
import net.codecadenza.eclipse.model.domain.AttributeTagEnumeration;
import net.codecadenza.eclipse.model.domain.CollectionMappingStrategyEnumeration;
import net.codecadenza.eclipse.model.domain.CollectionTypeEnumeration;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainAttributeValidator;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.DomainPackage;
import net.codecadenza.eclipse.model.domain.TemporalTypeEnumeration;
import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.PersistenceProviderEnumeration;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.util.JavaBeanHelper;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;
import org.eclipse.emf.ecore.util.EcoreUtil;

/**
 * An implementation of the model object '<em><b>Attribute</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.DomainAttributeImpl#getName <em>Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.DomainAttributeImpl#isPk <em>Pk</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.DomainAttributeImpl#getLabel <em>Label</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.DomainAttributeImpl#getLabelPlural <em>Label Plural</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.DomainAttributeImpl#isPersistent <em>Persistent</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.DomainAttributeImpl#isFetchTypeEager <em>Fetch Type Eager</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.DomainAttributeImpl#isInsertable <em>Insertable</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.DomainAttributeImpl#isUpdatable <em>Updatable</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.DomainAttributeImpl#isTrackVersion <em>Track Version</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.DomainAttributeImpl#isSetDateOnPersist <em>Set Date On Persist</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.DomainAttributeImpl#isSetDateOnUpdate <em>Set Date On Update</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.DomainAttributeImpl#isDisplayAttribute <em>Display Attribute</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.DomainAttributeImpl#getDomainObject <em>Domain Object</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.DomainAttributeImpl#getDomainAttributeValidator <em>Domain Attribute
 * Validator</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.DomainAttributeImpl#getTemporalType <em>Temporal Type</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.DomainAttributeImpl#getJavaType <em>Java Type</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.DomainAttributeImpl#getColumn <em>Column</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.DomainAttributeImpl#getTag <em>Tag</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.DomainAttributeImpl#isLob <em>Lob</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.DomainAttributeImpl#getInternalComment <em>Internal Comment</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.DomainAttributeImpl#getUserComment <em>User Comment</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.DomainAttributeImpl#isRemoveWhitespaceCharacters <em>Remove Whitespace
 * Characters</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.DomainAttributeImpl#isConvertToUpperCase <em>Convert To Upper
 * Case</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.DomainAttributeImpl#isConvertToLowerCase <em>Convert To Lower
 * Case</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.DomainAttributeImpl#getCollectionType <em>Collection Type</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.impl.DomainAttributeImpl#getCollectionMappingStrategy <em>Collection Mapping
 * Strategy</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public class DomainAttributeImpl extends EObjectImpl implements DomainAttribute {
	/**
	 * The default value of the '{@link #getName() <em>Name</em>}' attribute
	 * @see #getName()
	 * @generated
	 * @ordered
	 */
	protected static final String NAME_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getName() <em>Name</em>}' attribute
	 * @see #getName()
	 * @generated
	 * @ordered
	 */
	protected String name = NAME_EDEFAULT;

	/**
	 * The default value of the '{@link #isPk() <em>Pk</em>}' attribute
	 * @see #isPk()
	 * @generated
	 * @ordered
	 */
	protected static final boolean PK_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isPk() <em>Pk</em>}' attribute
	 * @see #isPk()
	 * @generated
	 * @ordered
	 */
	protected boolean pk = PK_EDEFAULT;

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
	 * The default value of the '{@link #isPersistent() <em>Persistent</em>}' attribute
	 * @see #isPersistent()
	 * @generated
	 * @ordered
	 */
	protected static final boolean PERSISTENT_EDEFAULT = true;

	/**
	 * The cached value of the '{@link #isPersistent() <em>Persistent</em>}' attribute
	 * @see #isPersistent()
	 * @generated
	 * @ordered
	 */
	protected boolean persistent = PERSISTENT_EDEFAULT;

	/**
	 * The default value of the '{@link #isFetchTypeEager() <em>Fetch Type Eager</em>}' attribute
	 * @see #isFetchTypeEager()
	 * @generated
	 * @ordered
	 */
	protected static final boolean FETCH_TYPE_EAGER_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isFetchTypeEager() <em>Fetch Type Eager</em>}' attribute
	 * @see #isFetchTypeEager()
	 * @generated
	 * @ordered
	 */
	protected boolean fetchTypeEager = FETCH_TYPE_EAGER_EDEFAULT;

	/**
	 * The default value of the '{@link #isInsertable() <em>Insertable</em>}' attribute
	 * @see #isInsertable()
	 * @generated
	 * @ordered
	 */
	protected static final boolean INSERTABLE_EDEFAULT = true;

	/**
	 * The cached value of the '{@link #isInsertable() <em>Insertable</em>}' attribute
	 * @see #isInsertable()
	 * @generated
	 * @ordered
	 */
	protected boolean insertable = INSERTABLE_EDEFAULT;

	/**
	 * The default value of the '{@link #isUpdatable() <em>Updatable</em>}' attribute
	 * @see #isUpdatable()
	 * @generated
	 * @ordered
	 */
	protected static final boolean UPDATABLE_EDEFAULT = true;

	/**
	 * The cached value of the '{@link #isUpdatable() <em>Updatable</em>}' attribute
	 * @see #isUpdatable()
	 * @generated
	 * @ordered
	 */
	protected boolean updatable = UPDATABLE_EDEFAULT;

	/**
	 * The default value of the '{@link #isTrackVersion() <em>Track Version</em>}' attribute
	 * @see #isTrackVersion()
	 * @generated
	 * @ordered
	 */
	protected static final boolean TRACK_VERSION_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isTrackVersion() <em>Track Version</em>}' attribute
	 * @see #isTrackVersion()
	 * @generated
	 * @ordered
	 */
	protected boolean trackVersion = TRACK_VERSION_EDEFAULT;

	/**
	 * The default value of the '{@link #isSetDateOnPersist() <em>Set Date On Persist</em>}' attribute
	 * @see #isSetDateOnPersist()
	 * @generated
	 * @ordered
	 */
	protected static final boolean SET_DATE_ON_PERSIST_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isSetDateOnPersist() <em>Set Date On Persist</em>}' attribute
	 * @see #isSetDateOnPersist()
	 * @generated
	 * @ordered
	 */
	protected boolean setDateOnPersist = SET_DATE_ON_PERSIST_EDEFAULT;

	/**
	 * The default value of the '{@link #isSetDateOnUpdate() <em>Set Date On Update</em>}' attribute
	 * @see #isSetDateOnUpdate()
	 * @generated
	 * @ordered
	 */
	protected static final boolean SET_DATE_ON_UPDATE_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isSetDateOnUpdate() <em>Set Date On Update</em>}' attribute
	 * @see #isSetDateOnUpdate()
	 * @generated
	 * @ordered
	 */
	protected boolean setDateOnUpdate = SET_DATE_ON_UPDATE_EDEFAULT;

	/**
	 * The default value of the '{@link #isDisplayAttribute() <em>Display Attribute</em>}' attribute
	 * @see #isDisplayAttribute()
	 * @generated
	 * @ordered
	 */
	protected static final boolean DISPLAY_ATTRIBUTE_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isDisplayAttribute() <em>Display Attribute</em>}' attribute
	 * @see #isDisplayAttribute()
	 * @generated
	 * @ordered
	 */
	protected boolean displayAttribute = DISPLAY_ATTRIBUTE_EDEFAULT;

	/**
	 * The cached value of the '{@link #getDomainAttributeValidator() <em>Domain Attribute Validator</em>}' containment reference
	 * @see #getDomainAttributeValidator()
	 * @generated
	 * @ordered
	 */
	protected DomainAttributeValidator domainAttributeValidator;

	/**
	 * The default value of the '{@link #getTemporalType() <em>Temporal Type</em>}' attribute
	 * @see #getTemporalType()
	 * @generated
	 * @ordered
	 */
	protected static final TemporalTypeEnumeration TEMPORAL_TYPE_EDEFAULT = TemporalTypeEnumeration.NONE;

	/**
	 * The cached value of the '{@link #getTemporalType() <em>Temporal Type</em>}' attribute
	 * @see #getTemporalType()
	 * @generated
	 * @ordered
	 */
	protected TemporalTypeEnumeration temporalType = TEMPORAL_TYPE_EDEFAULT;

	/**
	 * The cached value of the '{@link #getJavaType() <em>Java Type</em>}' reference
	 * @see #getJavaType()
	 * @generated
	 * @ordered
	 */
	protected JavaType javaType;

	/**
	 * The cached value of the '{@link #getColumn() <em>Column</em>}' reference
	 * @see #getColumn()
	 * @generated
	 * @ordered
	 */
	protected DBColumn column;

	/**
	 * The default value of the '{@link #getTag() <em>Tag</em>}' attribute
	 * @see #getTag()
	 * @generated
	 * @ordered
	 */
	protected static final AttributeTagEnumeration TAG_EDEFAULT = AttributeTagEnumeration.NONE;

	/**
	 * The cached value of the '{@link #getTag() <em>Tag</em>}' attribute
	 * @see #getTag()
	 * @generated
	 * @ordered
	 */
	protected AttributeTagEnumeration tag = TAG_EDEFAULT;

	/**
	 * The default value of the '{@link #isLob() <em>Lob</em>}' attribute
	 * @see #isLob()
	 * @generated
	 * @ordered
	 */
	protected static final boolean LOB_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isLob() <em>Lob</em>}' attribute
	 * @see #isLob()
	 * @generated
	 * @ordered
	 */
	protected boolean lob = LOB_EDEFAULT;

	/**
	 * The default value of the '{@link #getInternalComment() <em>Internal Comment</em>}' attribute
	 * @see #getInternalComment()
	 * @generated
	 * @ordered
	 */
	protected static final String INTERNAL_COMMENT_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getInternalComment() <em>Internal Comment</em>}' attribute
	 * @see #getInternalComment()
	 * @generated
	 * @ordered
	 */
	protected String internalComment = INTERNAL_COMMENT_EDEFAULT;

	/**
	 * The default value of the '{@link #getUserComment() <em>User Comment</em>}' attribute
	 * @see #getUserComment()
	 * @generated
	 * @ordered
	 */
	protected static final String USER_COMMENT_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getUserComment() <em>User Comment</em>}' attribute
	 * @see #getUserComment()
	 * @generated
	 * @ordered
	 */
	protected String userComment = USER_COMMENT_EDEFAULT;

	/**
	 * The default value of the '{@link #isRemoveWhitespaceCharacters() <em>Remove Whitespace Characters</em>}' attribute
	 * @see #isRemoveWhitespaceCharacters()
	 * @generated
	 * @ordered
	 */
	protected static final boolean REMOVE_WHITESPACE_CHARACTERS_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isRemoveWhitespaceCharacters() <em>Remove Whitespace Characters</em>}' attribute
	 * @see #isRemoveWhitespaceCharacters()
	 * @generated
	 * @ordered
	 */
	protected boolean removeWhitespaceCharacters = REMOVE_WHITESPACE_CHARACTERS_EDEFAULT;

	/**
	 * The default value of the '{@link #isConvertToUpperCase() <em>Convert To Upper Case</em>}' attribute
	 * @see #isConvertToUpperCase()
	 * @generated
	 * @ordered
	 */
	protected static final boolean CONVERT_TO_UPPER_CASE_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isConvertToUpperCase() <em>Convert To Upper Case</em>}' attribute
	 * @see #isConvertToUpperCase()
	 * @generated
	 * @ordered
	 */
	protected boolean convertToUpperCase = CONVERT_TO_UPPER_CASE_EDEFAULT;

	/**
	 * The default value of the '{@link #isConvertToLowerCase() <em>Convert To Lower Case</em>}' attribute
	 * @see #isConvertToLowerCase()
	 * @generated
	 * @ordered
	 */
	protected static final boolean CONVERT_TO_LOWER_CASE_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isConvertToLowerCase() <em>Convert To Lower Case</em>}' attribute
	 * @see #isConvertToLowerCase()
	 * @generated
	 * @ordered
	 */
	protected boolean convertToLowerCase = CONVERT_TO_LOWER_CASE_EDEFAULT;

	/**
	 * The default value of the '{@link #getCollectionType() <em>Collection Type</em>}' attribute
	 * @see #getCollectionType()
	 * @generated
	 * @ordered
	 */
	protected static final CollectionTypeEnumeration COLLECTION_TYPE_EDEFAULT = CollectionTypeEnumeration.NONE;

	/**
	 * The cached value of the '{@link #getCollectionType() <em>Collection Type</em>}' attribute
	 * @see #getCollectionType()
	 * @generated
	 * @ordered
	 */
	protected CollectionTypeEnumeration collectionType = COLLECTION_TYPE_EDEFAULT;

	/**
	 * The default value of the '{@link #getCollectionMappingStrategy() <em>Collection Mapping Strategy</em>}' attribute
	 * @see #getCollectionMappingStrategy()
	 * @generated
	 * @ordered
	 */
	protected static final CollectionMappingStrategyEnumeration COLLECTION_MAPPING_STRATEGY_EDEFAULT = CollectionMappingStrategyEnumeration.NONE;

	/**
	 * The cached value of the '{@link #getCollectionMappingStrategy() <em>Collection Mapping Strategy</em>}' attribute
	 * @see #getCollectionMappingStrategy()
	 * @generated
	 * @ordered
	 */
	protected CollectionMappingStrategyEnumeration collectionMappingStrategy = COLLECTION_MAPPING_STRATEGY_EDEFAULT;

	/**
	 * @generated
	 */
	protected DomainAttributeImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EObjectImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return DomainPackage.Literals.DOMAIN_ATTRIBUTE;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#getName()
	 * @generated
	 */
	@Override
	public String getName() {
		return name;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#setName(java.lang.String)
	 * @generated
	 */
	@Override
	public void setName(String newName) {
		final String oldName = name;
		name = newName;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.DOMAIN_ATTRIBUTE__NAME, oldName, name));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#isPk()
	 * @generated
	 */
	@Override
	public boolean isPk() {
		return pk;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#setPk(boolean)
	 * @generated
	 */
	@Override
	public void setPk(boolean newPk) {
		final boolean oldPk = pk;
		pk = newPk;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.DOMAIN_ATTRIBUTE__PK, oldPk, pk));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#getLabel()
	 * @generated
	 */
	@Override
	public String getLabel() {
		return label;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#setLabel(java.lang.String)
	 * @generated
	 */
	@Override
	public void setLabel(String newLabel) {
		final String oldLabel = label;
		label = newLabel;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.DOMAIN_ATTRIBUTE__LABEL, oldLabel, label));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#getLabelPlural()
	 * @generated
	 */
	@Override
	public String getLabelPlural() {
		return labelPlural;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#setLabelPlural(java.lang.String)
	 * @generated
	 */
	@Override
	public void setLabelPlural(String newLabelPlural) {
		final String oldLabelPlural = labelPlural;
		labelPlural = newLabelPlural;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.DOMAIN_ATTRIBUTE__LABEL_PLURAL, oldLabelPlural,
					labelPlural));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#isPersistent()
	 * @generated
	 */
	@Override
	public boolean isPersistent() {
		return persistent;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#setPersistent(boolean)
	 * @generated
	 */
	@Override
	public void setPersistent(boolean newPersistent) {
		final boolean oldPersistent = persistent;
		persistent = newPersistent;

		if (eNotificationRequired())
			eNotify(
					new ENotificationImpl(this, Notification.SET, DomainPackage.DOMAIN_ATTRIBUTE__PERSISTENT, oldPersistent, persistent));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#isFetchTypeEager()
	 * @generated
	 */
	@Override
	public boolean isFetchTypeEager() {
		return fetchTypeEager;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#setFetchTypeEager(boolean)
	 * @generated
	 */
	@Override
	public void setFetchTypeEager(boolean newFetchTypeEager) {
		final boolean oldFetchTypeEager = fetchTypeEager;
		fetchTypeEager = newFetchTypeEager;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.DOMAIN_ATTRIBUTE__FETCH_TYPE_EAGER, oldFetchTypeEager,
					fetchTypeEager));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#isInsertable()
	 * @generated
	 */
	@Override
	public boolean isInsertable() {
		return insertable;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#setInsertable(boolean)
	 * @generated
	 */
	@Override
	public void setInsertable(boolean newInsertable) {
		final boolean oldInsertable = insertable;
		insertable = newInsertable;

		if (eNotificationRequired())
			eNotify(
					new ENotificationImpl(this, Notification.SET, DomainPackage.DOMAIN_ATTRIBUTE__INSERTABLE, oldInsertable, insertable));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#isUpdatable()
	 * @generated
	 */
	@Override
	public boolean isUpdatable() {
		return updatable;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#setUpdatable(boolean)
	 * @generated
	 */
	@Override
	public void setUpdatable(boolean newUpdatable) {
		final boolean oldUpdatable = updatable;
		updatable = newUpdatable;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.DOMAIN_ATTRIBUTE__UPDATABLE, oldUpdatable, updatable));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#isTrackVersion()
	 * @generated
	 */
	@Override
	public boolean isTrackVersion() {
		return trackVersion;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#setTrackVersion(boolean)
	 * @generated
	 */
	@Override
	public void setTrackVersion(boolean newTrackVersion) {
		final boolean oldTrackVersion = trackVersion;
		trackVersion = newTrackVersion;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.DOMAIN_ATTRIBUTE__TRACK_VERSION, oldTrackVersion,
					trackVersion));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#isSetDateOnPersist()
	 * @generated
	 */
	@Override
	public boolean isSetDateOnPersist() {
		return setDateOnPersist;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#setSetDateOnPersist(boolean)
	 * @generated
	 */
	@Override
	public void setSetDateOnPersist(boolean newSetDateOnPersist) {
		final boolean oldSetDateOnPersist = setDateOnPersist;
		setDateOnPersist = newSetDateOnPersist;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.DOMAIN_ATTRIBUTE__SET_DATE_ON_PERSIST,
					oldSetDateOnPersist, setDateOnPersist));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#isSetDateOnUpdate()
	 * @generated
	 */
	@Override
	public boolean isSetDateOnUpdate() {
		return setDateOnUpdate;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#setSetDateOnUpdate(boolean)
	 * @generated
	 */
	@Override
	public void setSetDateOnUpdate(boolean newSetDateOnUpdate) {
		final boolean oldSetDateOnUpdate = setDateOnUpdate;
		setDateOnUpdate = newSetDateOnUpdate;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.DOMAIN_ATTRIBUTE__SET_DATE_ON_UPDATE,
					oldSetDateOnUpdate, setDateOnUpdate));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#isDisplayAttribute()
	 * @generated
	 */
	@Override
	public boolean isDisplayAttribute() {
		return displayAttribute;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#setDisplayAttribute(boolean)
	 * @generated
	 */
	@Override
	public void setDisplayAttribute(boolean newDisplayAttribute) {
		final boolean oldDisplayAttribute = displayAttribute;
		displayAttribute = newDisplayAttribute;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.DOMAIN_ATTRIBUTE__DISPLAY_ATTRIBUTE,
					oldDisplayAttribute, displayAttribute));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#getDomainObject()
	 * @generated
	 */
	@Override
	public DomainObject getDomainObject() {
		if (eContainerFeatureID() != DomainPackage.DOMAIN_ATTRIBUTE__DOMAIN_OBJECT)
			return null;

		return (DomainObject) eInternalContainer();
	}

	/**
	 * @param newDomainObject
	 * @param msgs
	 * @return a chain of notifications
	 * @generated
	 */
	public NotificationChain basicSetDomainObject(DomainObject newDomainObject, NotificationChain msgs) {
		msgs = eBasicSetContainer((InternalEObject) newDomainObject, DomainPackage.DOMAIN_ATTRIBUTE__DOMAIN_OBJECT, msgs);
		return msgs;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#setDomainObject(net.codecadenza.eclipse.model.domain.DomainObject)
	 * @generated
	 */
	@Override
	public void setDomainObject(DomainObject newDomainObject) {
		if (newDomainObject != eInternalContainer()
				|| (eContainerFeatureID() != DomainPackage.DOMAIN_ATTRIBUTE__DOMAIN_OBJECT && newDomainObject != null)) {
			if (EcoreUtil.isAncestor(this, newDomainObject))
				throw new IllegalArgumentException("Recursive containment not allowed for " + toString());

			NotificationChain msgs = null;

			if (eInternalContainer() != null)
				msgs = eBasicRemoveFromContainer(msgs);

			if (newDomainObject != null)
				msgs = ((InternalEObject) newDomainObject).eInverseAdd(this, DomainPackage.DOMAIN_OBJECT__ATTRIBUTES, DomainObject.class,
						msgs);

			msgs = basicSetDomainObject(newDomainObject, msgs);

			if (msgs != null)
				msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.DOMAIN_ATTRIBUTE__DOMAIN_OBJECT, newDomainObject,
					newDomainObject));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#getDomainAttributeValidator()
	 * @generated
	 */
	@Override
	public DomainAttributeValidator getDomainAttributeValidator() {
		return domainAttributeValidator;
	}

	/**
	 * @param newDomainAttributeValidator
	 * @param msgs
	 * @return a chain of notifications
	 * @generated
	 */
	public NotificationChain basicSetDomainAttributeValidator(DomainAttributeValidator newDomainAttributeValidator,
			NotificationChain msgs) {
		final DomainAttributeValidator oldDomainAttributeValidator = domainAttributeValidator;
		domainAttributeValidator = newDomainAttributeValidator;

		if (eNotificationRequired()) {
			final var notification = new ENotificationImpl(this, Notification.SET,
					DomainPackage.DOMAIN_ATTRIBUTE__DOMAIN_ATTRIBUTE_VALIDATOR, oldDomainAttributeValidator, newDomainAttributeValidator);

			if (msgs == null)
				msgs = notification;
			else
				msgs.add(notification);
		}

		return msgs;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#
	 * setDomainAttributeValidator(net.codecadenza.eclipse.model.domain.DomainAttributeValidator)
	 * @generated
	 */
	@Override
	public void setDomainAttributeValidator(DomainAttributeValidator newDomainAttributeValidator) {
		if (newDomainAttributeValidator != domainAttributeValidator) {
			NotificationChain msgs = null;

			if (domainAttributeValidator != null)
				msgs = ((InternalEObject) domainAttributeValidator).eInverseRemove(this,
						EOPPOSITE_FEATURE_BASE - DomainPackage.DOMAIN_ATTRIBUTE__DOMAIN_ATTRIBUTE_VALIDATOR, null, msgs);

			if (newDomainAttributeValidator != null)
				msgs = ((InternalEObject) newDomainAttributeValidator).eInverseAdd(this,
						EOPPOSITE_FEATURE_BASE - DomainPackage.DOMAIN_ATTRIBUTE__DOMAIN_ATTRIBUTE_VALIDATOR, null, msgs);

			msgs = basicSetDomainAttributeValidator(newDomainAttributeValidator, msgs);

			if (msgs != null)
				msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.DOMAIN_ATTRIBUTE__DOMAIN_ATTRIBUTE_VALIDATOR,
					newDomainAttributeValidator, newDomainAttributeValidator));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#getTemporalType()
	 * @generated
	 */
	@Override
	public TemporalTypeEnumeration getTemporalType() {
		return temporalType;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#setTemporalType(net.codecadenza.eclipse.model.domain.
	 * TemporalTypeEnumeration)
	 * @generated
	 */
	@Override
	public void setTemporalType(TemporalTypeEnumeration newTemporalType) {
		final TemporalTypeEnumeration oldTemporalType = temporalType;
		temporalType = newTemporalType == null ? TEMPORAL_TYPE_EDEFAULT : newTemporalType;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.DOMAIN_ATTRIBUTE__TEMPORAL_TYPE, oldTemporalType,
					temporalType));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#getJavaType()
	 * @generated
	 */
	@Override
	public JavaType getJavaType() {
		if (javaType != null && javaType.eIsProxy()) {
			final var oldJavaType = (InternalEObject) javaType;
			javaType = (JavaType) eResolveProxy(oldJavaType);

			if (javaType != oldJavaType && eNotificationRequired())
				eNotify(
						new ENotificationImpl(this, Notification.RESOLVE, DomainPackage.DOMAIN_ATTRIBUTE__JAVA_TYPE, oldJavaType, javaType));
		}

		return javaType;
	}

	/**
	 * @return the cached object instance
	 * @generated
	 */
	public JavaType basicGetJavaType() {
		return javaType;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#setJavaType(net.codecadenza.eclipse.model.java.JavaType)
	 * @generated
	 */
	@Override
	public void setJavaType(JavaType newJavaType) {
		final JavaType oldJavaType = javaType;
		javaType = newJavaType;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.DOMAIN_ATTRIBUTE__JAVA_TYPE, oldJavaType, javaType));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#getColumn()
	 * @generated
	 */
	@Override
	public DBColumn getColumn() {
		if (column != null && column.eIsProxy()) {
			final var oldColumn = (InternalEObject) column;
			column = (DBColumn) eResolveProxy(oldColumn);

			if (column != oldColumn && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, DomainPackage.DOMAIN_ATTRIBUTE__COLUMN, oldColumn, column));
		}

		return column;
	}

	/**
	 * @return the cached object instance
	 * @generated
	 */
	public DBColumn basicGetColumn() {
		return column;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#setColumn(net.codecadenza.eclipse.model.db.DBColumn)
	 * @generated
	 */
	@Override
	public void setColumn(DBColumn newColumn) {
		final DBColumn oldColumn = column;
		column = newColumn;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.DOMAIN_ATTRIBUTE__COLUMN, oldColumn, column));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#getTag()
	 * @generated
	 */
	@Override
	public AttributeTagEnumeration getTag() {
		return tag;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#
	 * setTag(net.codecadenza.eclipse.model.domain.AttributeTagEnumeration)
	 * @generated
	 */
	@Override
	public void setTag(AttributeTagEnumeration newTag) {
		final AttributeTagEnumeration oldTag = tag;
		tag = newTag == null ? TAG_EDEFAULT : newTag;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.DOMAIN_ATTRIBUTE__TAG, oldTag, tag));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#isLob()
	 * @generated
	 */
	@Override
	public boolean isLob() {
		return lob;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#setLob(boolean)
	 * @generated
	 */
	@Override
	public void setLob(boolean newLob) {
		final boolean oldLob = lob;
		lob = newLob;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.DOMAIN_ATTRIBUTE__LOB, oldLob, lob));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#getInternalComment()
	 * @generated
	 */
	@Override
	public String getInternalComment() {
		return internalComment;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#setInternalComment(java.lang.String)
	 * @generated
	 */
	@Override
	public void setInternalComment(String newInternalComment) {
		final String oldInternalComment = internalComment;
		internalComment = newInternalComment;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.DOMAIN_ATTRIBUTE__INTERNAL_COMMENT, oldInternalComment,
					internalComment));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#getUserComment()
	 * @generated
	 */
	@Override
	public String getUserComment() {
		return userComment;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#setUserComment(java.lang.String)
	 * @generated
	 */
	@Override
	public void setUserComment(String newUserComment) {
		final String oldUserComment = userComment;
		userComment = newUserComment;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.DOMAIN_ATTRIBUTE__USER_COMMENT, oldUserComment,
					userComment));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#isRemoveWhitespaceCharacters()
	 * @generated
	 */
	@Override
	public boolean isRemoveWhitespaceCharacters() {
		return removeWhitespaceCharacters;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#setRemoveWhitespaceCharacters(boolean)
	 * @generated
	 */
	@Override
	public void setRemoveWhitespaceCharacters(boolean newRemoveWhitespaceCharacters) {
		final boolean oldRemoveWhitespaceCharacters = removeWhitespaceCharacters;
		removeWhitespaceCharacters = newRemoveWhitespaceCharacters;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.DOMAIN_ATTRIBUTE__REMOVE_WHITESPACE_CHARACTERS,
					oldRemoveWhitespaceCharacters, removeWhitespaceCharacters));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#isConvertToUpperCase()
	 * @generated
	 */
	@Override
	public boolean isConvertToUpperCase() {
		return convertToUpperCase;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#setConvertToUpperCase(boolean)
	 * @generated
	 */
	@Override
	public void setConvertToUpperCase(boolean newConvertToUpperCase) {
		final boolean oldConvertToUpperCase = convertToUpperCase;
		convertToUpperCase = newConvertToUpperCase;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.DOMAIN_ATTRIBUTE__CONVERT_TO_UPPER_CASE,
					oldConvertToUpperCase, convertToUpperCase));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#isConvertToLowerCase()
	 * @generated
	 */
	@Override
	public boolean isConvertToLowerCase() {
		return convertToLowerCase;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#setConvertToLowerCase(boolean)
	 * @generated
	 */
	@Override
	public void setConvertToLowerCase(boolean newConvertToLowerCase) {
		final boolean oldConvertToLowerCase = convertToLowerCase;
		convertToLowerCase = newConvertToLowerCase;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.DOMAIN_ATTRIBUTE__CONVERT_TO_LOWER_CASE,
					oldConvertToLowerCase, convertToLowerCase));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#getCollectionType()
	 * @generated
	 */
	@Override
	public CollectionTypeEnumeration getCollectionType() {
		return collectionType;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#setCollectionType(net.codecadenza.eclipse.model.domain.
	 * CollectionTypeEnumeration)
	 * @generated
	 */
	@Override
	public void setCollectionType(CollectionTypeEnumeration newCollectionType) {
		final CollectionTypeEnumeration oldCollectionType = collectionType;
		collectionType = newCollectionType == null ? COLLECTION_TYPE_EDEFAULT : newCollectionType;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.DOMAIN_ATTRIBUTE__COLLECTION_TYPE, oldCollectionType,
					collectionType));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#getCollectionMappingStrategy()
	 * @generated
	 */
	@Override
	public CollectionMappingStrategyEnumeration getCollectionMappingStrategy() {
		return collectionMappingStrategy;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#setCollectionType(net.codecadenza.eclipse.model.domain.
	 * CollectionMappingStrategyEnumeration)
	 * @generated
	 */
	@Override
	public void setCollectionMappingStrategy(CollectionMappingStrategyEnumeration newCollectionMappingStrategy) {
		final CollectionMappingStrategyEnumeration oldCollectionMappingStrategy = collectionMappingStrategy;
		collectionMappingStrategy = newCollectionMappingStrategy == null ? COLLECTION_MAPPING_STRATEGY_EDEFAULT
				: newCollectionMappingStrategy;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DomainPackage.DOMAIN_ATTRIBUTE__COLLECTION_MAPPING_STRATEGY,
					oldCollectionMappingStrategy, collectionMappingStrategy));
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eInverseAdd(org.eclipse.emf.ecore.InternalEObject, int,
	 * org.eclipse.emf.common.notify.NotificationChain)
	 * @generated
	 */
	@Override
	public NotificationChain eInverseAdd(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case DomainPackage.DOMAIN_ATTRIBUTE__DOMAIN_OBJECT:
				if (eInternalContainer() != null)
					msgs = eBasicRemoveFromContainer(msgs);

				return basicSetDomainObject((DomainObject) otherEnd, msgs);
		}

		return super.eInverseAdd(otherEnd, featureID, msgs);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eInverseRemove(org.eclipse.emf.ecore.InternalEObject, int,
	 * org.eclipse.emf.common.notify.NotificationChain)
	 * @generated
	 */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case DomainPackage.DOMAIN_ATTRIBUTE__DOMAIN_OBJECT:
				return basicSetDomainObject(null, msgs);
			case DomainPackage.DOMAIN_ATTRIBUTE__DOMAIN_ATTRIBUTE_VALIDATOR:
				return basicSetDomainAttributeValidator(null, msgs);
		}

		return super.eInverseRemove(otherEnd, featureID, msgs);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eBasicRemoveFromContainerFeature(org.eclipse.emf.common.notify.
	 * NotificationChain)
	 * @generated
	 */
	@Override
	public NotificationChain eBasicRemoveFromContainerFeature(NotificationChain msgs) {
		switch (eContainerFeatureID()) {
			case DomainPackage.DOMAIN_ATTRIBUTE__DOMAIN_OBJECT:
				return eInternalContainer().eInverseRemove(this, DomainPackage.DOMAIN_OBJECT__ATTRIBUTES, DomainObject.class, msgs);
		}

		return super.eBasicRemoveFromContainerFeature(msgs);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eGet(int, boolean, boolean)
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case DomainPackage.DOMAIN_ATTRIBUTE__NAME:
				return getName();
			case DomainPackage.DOMAIN_ATTRIBUTE__PK:
				return isPk();
			case DomainPackage.DOMAIN_ATTRIBUTE__LABEL:
				return getLabel();
			case DomainPackage.DOMAIN_ATTRIBUTE__LABEL_PLURAL:
				return getLabelPlural();
			case DomainPackage.DOMAIN_ATTRIBUTE__PERSISTENT:
				return isPersistent();
			case DomainPackage.DOMAIN_ATTRIBUTE__FETCH_TYPE_EAGER:
				return isFetchTypeEager();
			case DomainPackage.DOMAIN_ATTRIBUTE__INSERTABLE:
				return isInsertable();
			case DomainPackage.DOMAIN_ATTRIBUTE__UPDATABLE:
				return isUpdatable();
			case DomainPackage.DOMAIN_ATTRIBUTE__TRACK_VERSION:
				return isTrackVersion();
			case DomainPackage.DOMAIN_ATTRIBUTE__SET_DATE_ON_PERSIST:
				return isSetDateOnPersist();
			case DomainPackage.DOMAIN_ATTRIBUTE__SET_DATE_ON_UPDATE:
				return isSetDateOnUpdate();
			case DomainPackage.DOMAIN_ATTRIBUTE__DISPLAY_ATTRIBUTE:
				return isDisplayAttribute();
			case DomainPackage.DOMAIN_ATTRIBUTE__DOMAIN_OBJECT:
				return getDomainObject();
			case DomainPackage.DOMAIN_ATTRIBUTE__DOMAIN_ATTRIBUTE_VALIDATOR:
				return getDomainAttributeValidator();
			case DomainPackage.DOMAIN_ATTRIBUTE__TEMPORAL_TYPE:
				return getTemporalType();
			case DomainPackage.DOMAIN_ATTRIBUTE__JAVA_TYPE:
				if (resolve)
					return getJavaType();

				return basicGetJavaType();
			case DomainPackage.DOMAIN_ATTRIBUTE__COLUMN:
				if (resolve)
					return getColumn();

				return basicGetColumn();
			case DomainPackage.DOMAIN_ATTRIBUTE__TAG:
				return getTag();
			case DomainPackage.DOMAIN_ATTRIBUTE__LOB:
				return isLob();
			case DomainPackage.DOMAIN_ATTRIBUTE__INTERNAL_COMMENT:
				return getInternalComment();
			case DomainPackage.DOMAIN_ATTRIBUTE__USER_COMMENT:
				return getUserComment();
			case DomainPackage.DOMAIN_ATTRIBUTE__REMOVE_WHITESPACE_CHARACTERS:
				return isRemoveWhitespaceCharacters();
			case DomainPackage.DOMAIN_ATTRIBUTE__CONVERT_TO_UPPER_CASE:
				return isConvertToUpperCase();
			case DomainPackage.DOMAIN_ATTRIBUTE__CONVERT_TO_LOWER_CASE:
				return isConvertToLowerCase();
			case DomainPackage.DOMAIN_ATTRIBUTE__COLLECTION_TYPE:
				return getCollectionType();
			case DomainPackage.DOMAIN_ATTRIBUTE__COLLECTION_MAPPING_STRATEGY:
				return getCollectionMappingStrategy();
		}

		return super.eGet(featureID, resolve, coreType);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eSet(int, java.lang.Object)
	 * @generated
	 */
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case DomainPackage.DOMAIN_ATTRIBUTE__NAME:
				setName((String) newValue);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE__PK:
				setPk((Boolean) newValue);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE__LABEL:
				setLabel((String) newValue);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE__LABEL_PLURAL:
				setLabelPlural((String) newValue);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE__PERSISTENT:
				setPersistent((Boolean) newValue);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE__FETCH_TYPE_EAGER:
				setFetchTypeEager((Boolean) newValue);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE__INSERTABLE:
				setInsertable((Boolean) newValue);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE__UPDATABLE:
				setUpdatable((Boolean) newValue);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE__TRACK_VERSION:
				setTrackVersion((Boolean) newValue);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE__SET_DATE_ON_PERSIST:
				setSetDateOnPersist((Boolean) newValue);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE__SET_DATE_ON_UPDATE:
				setSetDateOnUpdate((Boolean) newValue);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE__DISPLAY_ATTRIBUTE:
				setDisplayAttribute((Boolean) newValue);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE__DOMAIN_OBJECT:
				setDomainObject((DomainObject) newValue);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE__DOMAIN_ATTRIBUTE_VALIDATOR:
				setDomainAttributeValidator((DomainAttributeValidator) newValue);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE__TEMPORAL_TYPE:
				setTemporalType((TemporalTypeEnumeration) newValue);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE__JAVA_TYPE:
				setJavaType((JavaType) newValue);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE__COLUMN:
				setColumn((DBColumn) newValue);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE__TAG:
				setTag((AttributeTagEnumeration) newValue);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE__LOB:
				setLob((Boolean) newValue);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE__INTERNAL_COMMENT:
				setInternalComment((String) newValue);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE__USER_COMMENT:
				setUserComment((String) newValue);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE__REMOVE_WHITESPACE_CHARACTERS:
				setRemoveWhitespaceCharacters((boolean) newValue);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE__CONVERT_TO_UPPER_CASE:
				setConvertToUpperCase((boolean) newValue);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE__CONVERT_TO_LOWER_CASE:
				setConvertToLowerCase((boolean) newValue);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE__COLLECTION_TYPE:
				setCollectionType((CollectionTypeEnumeration) newValue);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE__COLLECTION_MAPPING_STRATEGY:
				setCollectionMappingStrategy((CollectionMappingStrategyEnumeration) newValue);
				return;
		}

		super.eSet(featureID, newValue);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eUnset(int)
	 * @generated
	 */
	@Override
	public void eUnset(int featureID) {
		switch (featureID) {
			case DomainPackage.DOMAIN_ATTRIBUTE__NAME:
				setName(NAME_EDEFAULT);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE__PK:
				setPk(PK_EDEFAULT);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE__LABEL:
				setLabel(LABEL_EDEFAULT);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE__LABEL_PLURAL:
				setLabelPlural(LABEL_PLURAL_EDEFAULT);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE__PERSISTENT:
				setPersistent(PERSISTENT_EDEFAULT);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE__FETCH_TYPE_EAGER:
				setFetchTypeEager(FETCH_TYPE_EAGER_EDEFAULT);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE__INSERTABLE:
				setInsertable(INSERTABLE_EDEFAULT);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE__UPDATABLE:
				setUpdatable(UPDATABLE_EDEFAULT);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE__TRACK_VERSION:
				setTrackVersion(TRACK_VERSION_EDEFAULT);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE__SET_DATE_ON_PERSIST:
				setSetDateOnPersist(SET_DATE_ON_PERSIST_EDEFAULT);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE__SET_DATE_ON_UPDATE:
				setSetDateOnUpdate(SET_DATE_ON_UPDATE_EDEFAULT);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE__DISPLAY_ATTRIBUTE:
				setDisplayAttribute(DISPLAY_ATTRIBUTE_EDEFAULT);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE__DOMAIN_OBJECT:
				setDomainObject((DomainObject) null);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE__DOMAIN_ATTRIBUTE_VALIDATOR:
				setDomainAttributeValidator((DomainAttributeValidator) null);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE__TEMPORAL_TYPE:
				setTemporalType(TEMPORAL_TYPE_EDEFAULT);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE__JAVA_TYPE:
				setJavaType((JavaType) null);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE__COLUMN:
				setColumn((DBColumn) null);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE__TAG:
				setTag(TAG_EDEFAULT);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE__LOB:
				setLob(LOB_EDEFAULT);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE__INTERNAL_COMMENT:
				setInternalComment(INTERNAL_COMMENT_EDEFAULT);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE__USER_COMMENT:
				setUserComment(USER_COMMENT_EDEFAULT);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE__REMOVE_WHITESPACE_CHARACTERS:
				setRemoveWhitespaceCharacters(REMOVE_WHITESPACE_CHARACTERS_EDEFAULT);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE__CONVERT_TO_UPPER_CASE:
				setConvertToUpperCase(CONVERT_TO_UPPER_CASE_EDEFAULT);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE__CONVERT_TO_LOWER_CASE:
				setConvertToLowerCase(CONVERT_TO_LOWER_CASE_EDEFAULT);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE__COLLECTION_TYPE:
				setCollectionType(COLLECTION_TYPE_EDEFAULT);
				return;
			case DomainPackage.DOMAIN_ATTRIBUTE__COLLECTION_MAPPING_STRATEGY:
				setCollectionMappingStrategy(COLLECTION_MAPPING_STRATEGY_EDEFAULT);
				return;
		}

		super.eUnset(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eIsSet(int)
	 * @generated
	 */
	@Override
	public boolean eIsSet(int featureID) {
		switch (featureID) {
			case DomainPackage.DOMAIN_ATTRIBUTE__NAME:
				return name != null;
			case DomainPackage.DOMAIN_ATTRIBUTE__PK:
				return pk != PK_EDEFAULT;
			case DomainPackage.DOMAIN_ATTRIBUTE__LABEL:
				return label != null;
			case DomainPackage.DOMAIN_ATTRIBUTE__LABEL_PLURAL:
				return labelPlural != null;
			case DomainPackage.DOMAIN_ATTRIBUTE__PERSISTENT:
				return persistent != PERSISTENT_EDEFAULT;
			case DomainPackage.DOMAIN_ATTRIBUTE__FETCH_TYPE_EAGER:
				return fetchTypeEager != FETCH_TYPE_EAGER_EDEFAULT;
			case DomainPackage.DOMAIN_ATTRIBUTE__INSERTABLE:
				return insertable != INSERTABLE_EDEFAULT;
			case DomainPackage.DOMAIN_ATTRIBUTE__UPDATABLE:
				return updatable != UPDATABLE_EDEFAULT;
			case DomainPackage.DOMAIN_ATTRIBUTE__TRACK_VERSION:
				return trackVersion != TRACK_VERSION_EDEFAULT;
			case DomainPackage.DOMAIN_ATTRIBUTE__SET_DATE_ON_PERSIST:
				return setDateOnPersist != SET_DATE_ON_PERSIST_EDEFAULT;
			case DomainPackage.DOMAIN_ATTRIBUTE__SET_DATE_ON_UPDATE:
				return setDateOnUpdate != SET_DATE_ON_UPDATE_EDEFAULT;
			case DomainPackage.DOMAIN_ATTRIBUTE__DISPLAY_ATTRIBUTE:
				return displayAttribute != DISPLAY_ATTRIBUTE_EDEFAULT;
			case DomainPackage.DOMAIN_ATTRIBUTE__DOMAIN_OBJECT:
				return getDomainObject() != null;
			case DomainPackage.DOMAIN_ATTRIBUTE__DOMAIN_ATTRIBUTE_VALIDATOR:
				return domainAttributeValidator != null;
			case DomainPackage.DOMAIN_ATTRIBUTE__TEMPORAL_TYPE:
				return temporalType != TEMPORAL_TYPE_EDEFAULT;
			case DomainPackage.DOMAIN_ATTRIBUTE__JAVA_TYPE:
				return javaType != null;
			case DomainPackage.DOMAIN_ATTRIBUTE__COLUMN:
				return column != null;
			case DomainPackage.DOMAIN_ATTRIBUTE__TAG:
				return tag != TAG_EDEFAULT;
			case DomainPackage.DOMAIN_ATTRIBUTE__LOB:
				return lob != LOB_EDEFAULT;
			case DomainPackage.DOMAIN_ATTRIBUTE__INTERNAL_COMMENT:
				return internalComment != null;
			case DomainPackage.DOMAIN_ATTRIBUTE__USER_COMMENT:
				return userComment != null;
			case DomainPackage.DOMAIN_ATTRIBUTE__REMOVE_WHITESPACE_CHARACTERS:
				return removeWhitespaceCharacters != REMOVE_WHITESPACE_CHARACTERS_EDEFAULT;
			case DomainPackage.DOMAIN_ATTRIBUTE__CONVERT_TO_UPPER_CASE:
				return convertToUpperCase != CONVERT_TO_UPPER_CASE_EDEFAULT;
			case DomainPackage.DOMAIN_ATTRIBUTE__CONVERT_TO_LOWER_CASE:
				return convertToLowerCase != CONVERT_TO_LOWER_CASE_EDEFAULT;
			case DomainPackage.DOMAIN_ATTRIBUTE__COLLECTION_TYPE:
				return collectionType != COLLECTION_TYPE_EDEFAULT;
			case DomainPackage.DOMAIN_ATTRIBUTE__COLLECTION_MAPPING_STRATEGY:
				return collectionMappingStrategy != COLLECTION_MAPPING_STRATEGY_EDEFAULT;
		}

		return super.eIsSet(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#toString()
	 * @generated
	 */
	@Override
	public String toString() {
		if (eIsProxy())
			return super.toString();

		final var result = new StringBuilder(super.toString());
		result.append(" (name: ");
		result.append(name);
		result.append(", pk: ");
		result.append(pk);
		result.append(", label: ");
		result.append(label);
		result.append(", labelPlural: ");
		result.append(labelPlural);
		result.append(", persistent: ");
		result.append(persistent);
		result.append(", fetchTypeEager: ");
		result.append(fetchTypeEager);
		result.append(", insertable: ");
		result.append(insertable);
		result.append(", updatable: ");
		result.append(updatable);
		result.append(", trackVersion: ");
		result.append(trackVersion);
		result.append(", setDateOnPersist: ");
		result.append(setDateOnPersist);
		result.append(", setDateOnUpdate: ");
		result.append(setDateOnUpdate);
		result.append(", displayAttribute: ");
		result.append(displayAttribute);
		result.append(", temporalType: ");
		result.append(temporalType);
		result.append(", tag: ");
		result.append(tag);
		result.append(", lob: ");
		result.append(lob);
		result.append(", internalComment: ");
		result.append(internalComment);
		result.append(", userComment: ");
		result.append(userComment);
		result.append(", removeWhitespaceCharacters: ");
		result.append(removeWhitespaceCharacters);
		result.append(", convertToUpperCase: ");
		result.append(convertToUpperCase);
		result.append(", convertToLowerCase: ");
		result.append(convertToLowerCase);
		result.append(", collectionType: ");
		result.append(collectionType);
		result.append(", collectionMappingStrategy: ");
		result.append(collectionMappingStrategy);
		result.append(')');

		return result.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#getUpperCaseName()
	 * @generated not
	 */
	@Override
	public String getUpperCaseName() {
		if (name.isEmpty())
			return "";

		if (name.length() == 1)
			return name.toUpperCase();

		return name.substring(0, 1).toUpperCase() + name.substring(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#getGetterName()
	 * @generated not
	 */
	@Override
	public String getGetterName() {
		return JavaBeanHelper.getGetterName(name, getJavaType().isType(JavaType.BOOL));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#getSetterName()
	 * @generated not
	 */
	@Override
	public String getSetterName() {
		return JavaBeanHelper.getSetterName(name);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#getGUILabel()
	 * @generated not
	 */
	@Override
	public String getGUILabel() {
		if (label.isEmpty())
			return "";

		return label.substring(0, 1).toUpperCase() + label.substring(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#convertAttribute()
	 * @generated not
	 */
	@Override
	public boolean convertAttribute() {
		// All currently available converters are only supported for attributes of type String!
		if (!getJavaType().isString() || getCollectionType() != CollectionTypeEnumeration.NONE)
			return false;

		return removeWhitespaceCharacters || convertToLowerCase || convertToUpperCase;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#getConverterExpression()
	 * @generated not
	 */
	@Override
	public String getConverterExpression() {
		var converter = "";

		if (!convertAttribute())
			return converter;

		if (removeWhitespaceCharacters)
			converter += ".trim()";

		if (convertToLowerCase)
			converter += ".toLowerCase()";
		else if (convertToUpperCase)
			converter += ".toUpperCase()";

		return converter;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#getGetterReference()
	 * @generated not
	 */
	@Override
	public String getGetterReference() {
		return JavaBeanHelper.getGetterReference(name, getJavaType().isType(JavaType.BOOL));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#getSetterReference()
	 * @generated not
	 */
	@Override
	public String getSetterReference() {
		return "::" + getSetterName();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#getSearchFieldDataType()
	 * @generated not
	 */
	@Override
	public String getSearchFieldDataType() {
		if (getJavaType().isBoolean())
			return "SearchFieldDataTypeEnum.BOOLEAN";
		else if (getJavaType().isString())
			return "SearchFieldDataTypeEnum.STRING";
		else if (getJavaType().isChar())
			return "SearchFieldDataTypeEnum.CHAR";
		else if (getJavaType().isInteger())
			return "SearchFieldDataTypeEnum.INTEGER";
		else if (getJavaType().isLong())
			return "SearchFieldDataTypeEnum.LONG";
		else if (getJavaType().isDouble())
			return "SearchFieldDataTypeEnum.DOUBLE";
		else if (getJavaType().isFloat())
			return "SearchFieldDataTypeEnum.FLOAT";
		else if (getJavaType().isBigDecimal())
			return "SearchFieldDataTypeEnum.BIG_DECIMAL";
		else if (getJavaType().isDate())
			return "SearchFieldDataTypeEnum.DATE";
		else if (getJavaType().isCalendar())
			return "SearchFieldDataTypeEnum.GREGORIAN_CALENDAR";
		else if (getJavaType().isLocalDate())
			return "SearchFieldDataTypeEnum.LOCAL_DATE";
		else if (getJavaType().isLocalDateTime())
			return "SearchFieldDataTypeEnum.LOCAL_DATE_TIME";
		else if (getJavaType().isEnum())
			return "SearchFieldDataTypeEnum.ENUM";
		else if (getJavaType().isUUID()) {
			if (isWildcardFilteringSupported())
				return "SearchFieldDataTypeEnum.UUID_STRING";

			return "SearchFieldDataTypeEnum.UUID_BINARY";
		}

		throw new IllegalStateException("The type '" + getJavaType().getName() + "' is not supported!");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#convertToString(java.lang.String)
	 * @generated not
	 */
	@Override
	public String convertToString(String expression) {
		if (getJavaType().isString())
			return expression;
		else if (getJavaType().isInteger())
			return "Integer.toString(" + expression + ")";
		else if (getJavaType().isLong())
			return "Long.toString(" + expression + ")";
		else if (getJavaType().isChar())
			return "Character.toString(" + expression + ")";
		else if (getJavaType().isBoolean())
			return "Boolean.toString(" + expression + ")";
		else if (getJavaType().isDecimalNumber())
			return "decimalFormat.format(" + expression + ")";
		else if (getJavaType().isUUID())
			return expression + ".toString()";
		else if (getJavaType().isTemporalType()) {
			if (getJavaType().isLocalDate() || getTemporalType() == TemporalTypeEnumeration.DATE) {
				if (getJavaType().isDateOrCalendar())
					return "dateFormat.format(" + convertToInstant(expression) + ")";

				return "dateFormat.format(" + expression + ")";
			}
			else if (getJavaType().isDateOrCalendar())
				return "dateTimeFormat.format(" + convertToInstant(expression) + ")";
			else
				return "dateTimeFormat.format(" + expression + ")";
		}
		else if (getJavaType().isEnum()) {
			final Project project = getDomainObject().getNamespace().getProject();
			final var javaEnum = (JavaEnum) getJavaType();

			if (project.hasVaadinClient())
				return "i18n.getTranslation(\"" + javaEnum.getName().toLowerCase() + "_\" + " + expression + ".name().toLowerCase())";
			else if (project.hasJSFClient())
				return "bundle.getString(\"" + javaEnum.getName().toLowerCase() + "_\" + " + expression + ".name().toLowerCase())";
			else
				return "getTranslation(\"" + javaEnum.getName().toLowerCase() + "_\" + " + expression + ".name().toLowerCase())";
		}

		throw new IllegalStateException("The type '" + getJavaType().getName() + "' is not supported!");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#convertFromString(java.lang.String)
	 * @generated not
	 */
	@Override
	public String convertFromString(String expression) {
		if (getJavaType().isString())
			return expression;
		else if (getJavaType().isInteger())
			return "Integer.parseInt(" + expression + ")";
		else if (getJavaType().isLong())
			return "Long.parseLong(" + expression + ")";
		else if (getJavaType().isUUID())
			return "java.util.UUID.fromString(" + expression + ")";

		throw new IllegalStateException("The type '" + getJavaType().getName() + "' is not supported!");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#convertToInstant(java.lang.String)
	 * @generated not
	 */
	@Override
	public String convertToInstant(String expression) {
		final Project project = getDomainObject().getNamespace().getProject();
		final boolean usesHibernate = project.getPersistenceProvider() == PersistenceProviderEnumeration.HIBERNATE;

		// Internally, Hibernate uses the class java.sql.Date for the mapping of date fields. The method toInstant() must not be used
		// as it always throws an UnsupportedOperationException!
		if (getJavaType().isDate() && getTemporalType() == TemporalTypeEnumeration.DATE && usesHibernate)
			return PACK_JAVA_TIME + ".Instant.ofEpochMilli(" + expression + ".getTime())";

		return expression + ".toInstant()";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#getEmptyItemDefaultValue()
	 * @generated not
	 */
	@Override
	public String getEmptyItemDefaultValue() {
		if (getJavaType().isString())
			return "\"\"";
		else if (getJavaType().isInteger())
			return "Integer.MIN_VALUE";
		else if (getJavaType().isLong())
			return "Long.MIN_VALUE";
		else
			return "null";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#isWildcardFilteringSupported()
	 * @generated not
	 */
	@Override
	public boolean isWildcardFilteringSupported() {
		if (getJavaType().isString())
			return true;

		if (getJavaType().isUUID() && column != null) {
			// Check if the corresponding column type can also be used for attributes of type String. If the column type cannot be used
			// for strings it is assumed that it is used for binary data and this type cannot be used for filter operations with
			// wildcards!
			return column.getColumnType().getJavaTypes().stream().anyMatch(type -> type.getName().equals(JavaType.STRING));
		}

		return false;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#getMinFieldLength()
	 * @generated not
	 */
	@Override
	public Optional<Integer> getMinFieldLength() {
		final Integer minLength = getDomainAttributeValidator().getMinLength();

		if (minLength == null || minLength == 0)
			return Optional.empty();

		return Optional.of(minLength);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#getMaxFieldLenght()
	 * @generated not
	 */
	@Override
	public Optional<Integer> getMaxFieldLenght() {
		if (getDomainAttributeValidator().getMaxLength() == null)
			return Optional.empty();

		return Optional.of(getDomainAttributeValidator().getMaxLength());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#getMaxFileSize()
	 * @generated not
	 */
	@Override
	public String getMaxFileSize() {
		final Integer maxSize = getDomainAttributeValidator().getMaxLength();

		if (isLob() && maxSize != null)
			return Integer.toString(maxSize);

		return "Integer.MAX_VALUE";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#getTypeName()
	 * @generated not
	 */
	@Override
	public String getTypeName() {
		if (getCollectionType() == CollectionTypeEnumeration.NONE)
			return getJavaType().getName();
		else if (getCollectionType() == CollectionTypeEnumeration.LIST)
			return "List<" + getJavaType().getWrapperTypeName() + ">";
		else
			return "Set<" + getJavaType().getWrapperTypeName() + ">";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#getCollectionTable()
	 * @generated not
	 */
	@Override
	public DBTable getCollectionTable() {
		if (getCollectionMappingStrategy() == CollectionMappingStrategyEnumeration.TABLE)
			return getColumn().getDatabaseTable();

		return null;
	}

}
