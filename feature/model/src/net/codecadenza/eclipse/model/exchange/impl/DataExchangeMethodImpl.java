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

import static net.codecadenza.eclipse.shared.Constants.EXCHANGE_PATH_PARAM;

import java.nio.charset.StandardCharsets;
import java.util.Collection;
import net.codecadenza.eclipse.model.exchange.AssociationController;
import net.codecadenza.eclipse.model.exchange.ContentTypeEnumeration;
import net.codecadenza.eclipse.model.exchange.DataExchangeElement;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethodTypeEnumeration;
import net.codecadenza.eclipse.model.exchange.DataExchangeMode;
import net.codecadenza.eclipse.model.exchange.DataExchangeServiceBean;
import net.codecadenza.eclipse.model.exchange.ExchangePackage;
import net.codecadenza.eclipse.model.exchange.FileExchangeMode;
import net.codecadenza.eclipse.model.exchange.FilterMethodParameter;
import net.codecadenza.eclipse.model.exchange.ParserImplementationEnumeration;
import net.codecadenza.eclipse.model.java.MethodParameter;
import net.codecadenza.eclipse.model.service.impl.ServiceMethodImpl;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.emf.ecore.util.InternalEList;

/**
 * An implementation of the model object '<em><b>Data Exchange Method</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeMethodImpl#getExchangeMode <em>Exchange Mode</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeMethodImpl#getRootElement <em>Root Element</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeMethodImpl#getContentType <em>Content Type</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeMethodImpl#getMethodType <em>Method Type</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeMethodImpl#getParser <em>Parser</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeMethodImpl#isPerformValidation <em>Perform
 * Validation</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeMethodImpl#getCharset <em>Charset</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeMethodImpl#isFormatOutput <em>Format Output</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeMethodImpl#getDataExchangeServiceBean <em>Data Exchange
 * Service Bean</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeMethodImpl#isProcessSingleObject <em>Process Single
 * Object</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeMethodImpl#getJoinedImportMethod <em>Joined Import
 * Method</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeMethodImpl#getQuoteCharacter <em>Quote Character</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeMethodImpl#getRecordSeparator <em>Record
 * Separator</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeMethodImpl#getCommentCharacter <em>Comment
 * Character</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeMethodImpl#getDelimiter <em>Delimiter</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeMethodImpl#getDefaultDateFormat <em>Default Date
 * Format</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeMethodImpl#getDefaultDateTimeFormat <em>Default Date Time
 * Format</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeMethodImpl#getDefaultNumberFormat <em>Default Number
 * Format</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeMethodImpl#getAssociationControllers <em>Association
 * Controllers</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeMethodImpl#getSchemaFileName() <em>Schema File
 * Name</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public class DataExchangeMethodImpl extends ServiceMethodImpl implements DataExchangeMethod {
	/**
	 * The cached value of the '{@link #getExchangeMode() <em>Exchange Mode</em>}' containment reference
	 * @see #getExchangeMode()
	 * @generated
	 * @ordered
	 */
	protected DataExchangeMode exchangeMode;

	/**
	 * The cached value of the '{@link #getRootElement() <em>Root Element</em>}' containment reference
	 * @see #getRootElement()
	 * @generated
	 * @ordered
	 */
	protected DataExchangeElement rootElement;

	/**
	 * The default value of the '{@link #getContentType() <em>Content Type</em>}' attribute
	 * @see #getContentType()
	 * @generated
	 * @ordered
	 */
	protected static final ContentTypeEnumeration CONTENT_TYPE_EDEFAULT = ContentTypeEnumeration.XML;

	/**
	 * The cached value of the '{@link #getContentType() <em>Content Type</em>}' attribute
	 * @see #getContentType()
	 * @generated
	 * @ordered
	 */
	protected ContentTypeEnumeration contentType = CONTENT_TYPE_EDEFAULT;

	/**
	 * The default value of the '{@link #getMethodType() <em>Method Type</em>}' attribute
	 * @see #getMethodType()
	 * @generated
	 * @ordered
	 */
	protected static final DataExchangeMethodTypeEnumeration METHOD_TYPE_EDEFAULT = DataExchangeMethodTypeEnumeration.IMPORT;

	/**
	 * The cached value of the '{@link #getMethodType() <em>Method Type</em>}' attribute
	 * @see #getMethodType()
	 * @generated
	 * @ordered
	 */
	protected DataExchangeMethodTypeEnumeration methodType = METHOD_TYPE_EDEFAULT;

	/**
	 * The default value of the '{@link #getParser() <em>Parser</em>}' attribute
	 * @see #getParser()
	 * @generated
	 * @ordered
	 */
	protected static final ParserImplementationEnumeration PARSER_EDEFAULT = ParserImplementationEnumeration.POI;

	/**
	 * The cached value of the '{@link #getParser() <em>Parser</em>}' attribute
	 * @see #getParser()
	 * @generated
	 * @ordered
	 */
	protected ParserImplementationEnumeration parser = PARSER_EDEFAULT;

	/**
	 * The default value of the '{@link #isPerformValidation() <em>Perform Validation</em>}' attribute
	 * @see #isPerformValidation()
	 * @generated
	 * @ordered
	 */
	protected static final boolean PERFORM_VALIDATION_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isPerformValidation() <em>Perform Validation</em>}' attribute
	 * @see #isPerformValidation()
	 * @generated
	 * @ordered
	 */
	protected boolean performValidation = PERFORM_VALIDATION_EDEFAULT;

	/**
	 * The default value of the '{@link #getCharset() <em>Charset</em>}' attribute
	 * @see #getCharset()
	 * @generated
	 * @ordered
	 */
	protected static final String CHARSET_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getCharset() <em>Charset</em>}' attribute
	 * @see #getCharset()
	 * @generated
	 * @ordered
	 */
	protected String charset = CHARSET_EDEFAULT;

	/**
	 * The default value of the '{@link #isFormatOutput() <em>Format Output</em>}' attribute
	 * @see #isFormatOutput()
	 * @generated
	 * @ordered
	 */
	protected static final boolean FORMAT_OUTPUT_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isFormatOutput() <em>Format Output</em>}' attribute
	 * @see #isFormatOutput()
	 * @generated
	 * @ordered
	 */
	protected boolean formatOutput = FORMAT_OUTPUT_EDEFAULT;

	/**
	 * The default value of the '{@link #isProcessSingleObject() <em>Process Single Object</em>}' attribute
	 * @see #isProcessSingleObject()
	 * @generated
	 * @ordered
	 */
	protected static final boolean PROCESS_SINGLE_OBJECT_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isProcessSingleObject() <em>Process Single Object</em>}' attribute
	 * @see #isProcessSingleObject()
	 * @generated
	 * @ordered
	 */
	protected boolean processSingleObject = PROCESS_SINGLE_OBJECT_EDEFAULT;

	/**
	 * The cached value of the '{@link #getJoinedImportMethod() <em>Joined Import Method</em>}' reference
	 * @see #getJoinedImportMethod()
	 * @generated
	 * @ordered
	 */
	protected DataExchangeMethod joinedImportMethod;

	/**
	 * The default value of the '{@link #getQuoteCharacter() <em>Quote Character</em>}' attribute
	 * @see #getQuoteCharacter()
	 * @generated
	 * @ordered
	 */
	protected static final char QUOTE_CHARACTER_EDEFAULT = '\u0000';

	/**
	 * The cached value of the '{@link #getQuoteCharacter() <em>Quote Character</em>}' attribute
	 * @see #getQuoteCharacter()
	 * @generated
	 * @ordered
	 */
	protected char quoteCharacter = QUOTE_CHARACTER_EDEFAULT;

	/**
	 * The default value of the '{@link #getRecordSeparator() <em>Record Separator</em>}' attribute
	 * @see #getRecordSeparator()
	 * @generated
	 * @ordered
	 */
	protected static final String RECORD_SEPARATOR_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getRecordSeparator() <em>Record Separator</em>}' attribute
	 * @see #getRecordSeparator()
	 * @generated
	 * @ordered
	 */
	protected String recordSeparator = RECORD_SEPARATOR_EDEFAULT;

	/**
	 * The default value of the '{@link #getCommentCharacter() <em>Comment Character</em>}' attribute
	 * @see #getCommentCharacter()
	 * @generated
	 * @ordered
	 */
	protected static final char COMMENT_CHARACTER_EDEFAULT = '\u0000';

	/**
	 * The cached value of the '{@link #getCommentCharacter() <em>Comment Character</em>}' attribute
	 * @see #getCommentCharacter()
	 * @generated
	 * @ordered
	 */
	protected char commentCharacter = COMMENT_CHARACTER_EDEFAULT;

	/**
	 * The default value of the '{@link #getDelimiter() <em>Delimiter</em>}' attribute
	 * @see #getDelimiter()
	 * @generated
	 * @ordered
	 */
	protected static final char DELIMITER_EDEFAULT = '\u0000';

	/**
	 * The cached value of the '{@link #getDelimiter() <em>Delimiter</em>}' attribute
	 * @see #getDelimiter()
	 * @generated
	 * @ordered
	 */
	protected char delimiter = DELIMITER_EDEFAULT;

	/**
	 * The default value of the '{@link #getDefaultDateFormat() <em>Default Date Format</em>}' attribute
	 * @see #getDefaultDateFormat()
	 * @generated
	 * @ordered
	 */
	protected static final String DEFAULT_DATE_FORMAT_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getDefaultDateFormat() <em>Default Date Format</em>}' attribute
	 * @see #getDefaultDateFormat()
	 * @generated
	 * @ordered
	 */
	protected String defaultDateFormat = DEFAULT_DATE_FORMAT_EDEFAULT;

	/**
	 * The default value of the '{@link #getDefaultDateTimeFormat() <em>Default Date Time Format</em>}' attribute
	 * @see #getDefaultDateTimeFormat()
	 * @generated
	 * @ordered
	 */
	protected static final String DEFAULT_DATE_TIME_FORMAT_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getDefaultDateTimeFormat() <em>Default Date Time Format</em>}' attribute
	 * @see #getDefaultDateTimeFormat()
	 * @generated
	 * @ordered
	 */
	protected String defaultDateTimeFormat = DEFAULT_DATE_TIME_FORMAT_EDEFAULT;

	/**
	 * The default value of the '{@link #getDefaultNumberFormat() <em>Default Number Format</em>}' attribute
	 * @see #getDefaultNumberFormat()
	 * @generated
	 * @ordered
	 */
	protected static final String DEFAULT_NUMBER_FORMAT_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getDefaultNumberFormat() <em>Default Number Format</em>}' attribute
	 * @see #getDefaultNumberFormat()
	 * @generated
	 * @ordered
	 */
	protected String defaultNumberFormat = DEFAULT_NUMBER_FORMAT_EDEFAULT;

	/**
	 * The cached value of the '{@link #getAssociationControllers() <em>Association Controllers</em>}' containment reference list
	 * @see #getAssociationControllers()
	 * @generated
	 * @ordered
	 */
	protected EList<AssociationController> associationControllers;

	/**
	 * The default value of the '{@link #getSchemaFileName() <em>Schema File Name</em>}' attribute
	 * @see #getSchemaFileName()
	 * @generated
	 * @ordered
	 */
	protected static final String SCHEMA_FILE_NAME_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getSchemaFileName() <em>Schema File Name</em>}' attribute
	 * @see #getSchemaFileName()
	 * @generated
	 * @ordered
	 */
	protected String schemaFileName = SCHEMA_FILE_NAME_EDEFAULT;

	/**
	 * @generated
	 */
	protected DataExchangeMethodImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.impl.ServiceMethodImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return ExchangePackage.Literals.DATA_EXCHANGE_METHOD;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getDataExchangeServiceBean()
	 * @generated
	 */
	@Override
	public DataExchangeServiceBean getDataExchangeServiceBean() {
		if (eContainerFeatureID() != ExchangePackage.DATA_EXCHANGE_METHOD__DATA_EXCHANGE_SERVICE_BEAN)
			return null;

		return (DataExchangeServiceBean) eInternalContainer();
	}

	/**
	 * @param newDataExchangeServiceBean
	 * @param msgs
	 * @return a chain of notifications
	 * @generated
	 */
	public NotificationChain basicSetDataExchangeServiceBean(DataExchangeServiceBean newDataExchangeServiceBean,
			NotificationChain msgs) {
		msgs = eBasicSetContainer((InternalEObject) newDataExchangeServiceBean,
				ExchangePackage.DATA_EXCHANGE_METHOD__DATA_EXCHANGE_SERVICE_BEAN, msgs);
		return msgs;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#setDataExchangeServiceBean(net.codecadenza.eclipse.model.
	 * exchange.DataExchangeServiceBean)
	 * @generated
	 */
	@Override
	public void setDataExchangeServiceBean(DataExchangeServiceBean newDataExchangeServiceBean) {
		if (newDataExchangeServiceBean != eInternalContainer()
				|| (eContainerFeatureID() != ExchangePackage.DATA_EXCHANGE_METHOD__DATA_EXCHANGE_SERVICE_BEAN
						&& newDataExchangeServiceBean != null)) {
			if (EcoreUtil.isAncestor(this, newDataExchangeServiceBean))
				throw new IllegalArgumentException("Recursive containment not allowed for " + toString());

			NotificationChain msgs = null;

			if (eInternalContainer() != null)
				msgs = eBasicRemoveFromContainer(msgs);

			if (newDataExchangeServiceBean != null)
				msgs = ((InternalEObject) newDataExchangeServiceBean).eInverseAdd(this,
						ExchangePackage.DATA_EXCHANGE_SERVICE_BEAN__DATA_EXCHANGE_METHODS, DataExchangeServiceBean.class, msgs);

			msgs = basicSetDataExchangeServiceBean(newDataExchangeServiceBean, msgs);

			if (msgs != null)
				msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.DATA_EXCHANGE_METHOD__DATA_EXCHANGE_SERVICE_BEAN,
					newDataExchangeServiceBean, newDataExchangeServiceBean));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#isProcessSingleObject()
	 * @generated
	 */
	@Override
	public boolean isProcessSingleObject() {
		return processSingleObject;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#setProcessSingleObject(boolean)
	 * @generated
	 */
	@Override
	public void setProcessSingleObject(boolean newProcessSingleObject) {
		final boolean oldProcessSingleObject = processSingleObject;
		processSingleObject = newProcessSingleObject;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.DATA_EXCHANGE_METHOD__PROCESS_SINGLE_OBJECT,
					oldProcessSingleObject, processSingleObject));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getJoinedImportMethod()
	 * @generated
	 */
	@Override
	public DataExchangeMethod getJoinedImportMethod() {
		if (joinedImportMethod != null && joinedImportMethod.eIsProxy()) {
			final var oldJoinedImportMethod = (InternalEObject) joinedImportMethod;
			joinedImportMethod = (DataExchangeMethod) eResolveProxy(oldJoinedImportMethod);

			if (joinedImportMethod != oldJoinedImportMethod && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, ExchangePackage.DATA_EXCHANGE_METHOD__JOINED_IMPORT_METHOD,
						oldJoinedImportMethod, joinedImportMethod));
		}

		return joinedImportMethod;
	}

	/**
	 * @return the cached object instance
	 * @generated
	 */
	public DataExchangeMethod basicGetJoinedImportMethod() {
		return joinedImportMethod;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#setJoinedImportMethod(net.codecadenza.eclipse.model.exchange.
	 * DataExchangeMethod)
	 * @generated
	 */
	@Override
	public void setJoinedImportMethod(DataExchangeMethod newJoinedImportMethod) {
		final DataExchangeMethod oldJoinedImportMethod = joinedImportMethod;
		joinedImportMethod = newJoinedImportMethod;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.DATA_EXCHANGE_METHOD__JOINED_IMPORT_METHOD,
					oldJoinedImportMethod, joinedImportMethod));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getQuoteCharacter()
	 * @generated
	 */
	@Override
	public char getQuoteCharacter() {
		return quoteCharacter;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#setQuoteCharacter(char)
	 * @generated
	 */
	@Override
	public void setQuoteCharacter(char newQuoteCharacter) {
		final char oldQuoteCharacter = quoteCharacter;
		quoteCharacter = newQuoteCharacter;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.DATA_EXCHANGE_METHOD__QUOTE_CHARACTER,
					oldQuoteCharacter, quoteCharacter));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getRecordSeparator()
	 * @generated
	 */
	@Override
	public String getRecordSeparator() {
		return recordSeparator;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#setRecordSeparator(java.lang.String)
	 * @generated
	 */
	@Override
	public void setRecordSeparator(String newRecordSeparator) {
		final String oldRecordSeparator = recordSeparator;
		recordSeparator = newRecordSeparator;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.DATA_EXCHANGE_METHOD__RECORD_SEPARATOR,
					oldRecordSeparator, recordSeparator));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getCommentCharacter()
	 * @generated
	 */
	@Override
	public char getCommentCharacter() {
		return commentCharacter;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#setCommentCharacter(char)
	 * @generated
	 */
	@Override
	public void setCommentCharacter(char newCommentCharacter) {
		final char oldCommentCharacter = commentCharacter;
		commentCharacter = newCommentCharacter;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.DATA_EXCHANGE_METHOD__COMMENT_CHARACTER,
					oldCommentCharacter, commentCharacter));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getDelimiter()
	 * @generated
	 */
	@Override
	public char getDelimiter() {
		return delimiter;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#setDelimiter(char)
	 * @generated
	 */
	@Override
	public void setDelimiter(char newDelimiter) {
		final char oldDelimiter = delimiter;
		delimiter = newDelimiter;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.DATA_EXCHANGE_METHOD__DELIMITER, oldDelimiter,
					delimiter));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getDefaultDateFormat()
	 * @generated
	 */
	@Override
	public String getDefaultDateFormat() {
		return defaultDateFormat;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#setDefaultDateFormat(java.lang.String)
	 * @generated
	 */
	@Override
	public void setDefaultDateFormat(String newDefaultDateFormat) {
		final String oldDefaultDateFormat = defaultDateFormat;
		defaultDateFormat = newDefaultDateFormat;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.DATA_EXCHANGE_METHOD__DEFAULT_DATE_FORMAT,
					oldDefaultDateFormat, defaultDateFormat));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getDefaultDateTimeFormat()
	 * @generated
	 */
	@Override
	public String getDefaultDateTimeFormat() {
		return defaultDateTimeFormat;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#setDefaultDateTimeFormat(java.lang.String)
	 * @generated
	 */
	@Override
	public void setDefaultDateTimeFormat(String newDefaultDateTimeFormat) {
		final String oldDefaultDateTimeFormat = defaultDateTimeFormat;
		defaultDateTimeFormat = newDefaultDateTimeFormat;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.DATA_EXCHANGE_METHOD__DEFAULT_DATE_TIME_FORMAT,
					oldDefaultDateTimeFormat, defaultDateTimeFormat));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getDefaultNumberFormat()
	 * @generated
	 */
	@Override
	public String getDefaultNumberFormat() {
		return defaultNumberFormat;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#setDefaultNumberFormat(java.lang.String)
	 * @generated
	 */
	@Override
	public void setDefaultNumberFormat(String newDefaultNumberFormat) {
		final String oldDefaultNumberFormat = defaultNumberFormat;
		defaultNumberFormat = newDefaultNumberFormat;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.DATA_EXCHANGE_METHOD__DEFAULT_NUMBER_FORMAT,
					oldDefaultNumberFormat, defaultNumberFormat));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getAssociationControllers()
	 * @generated
	 */
	@Override
	public EList<AssociationController> getAssociationControllers() {
		if (associationControllers == null)
			associationControllers = new EObjectContainmentEList<>(AssociationController.class, this,
					ExchangePackage.DATA_EXCHANGE_METHOD__ASSOCIATION_CONTROLLERS);

		return associationControllers;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getSchemaFileName()
	 * @generated
	 */
	@Override
	public String getSchemaFileName() {
		return schemaFileName;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#setSchemaFileName(java.lang.String)
	 * @generated
	 */
	@Override
	public void setSchemaFileName(String newSchemaFileName) {
		final String oldSchemaFileName = schemaFileName;
		schemaFileName = newSchemaFileName;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.DATA_EXCHANGE_METHOD__SCHEMA_FILE_NAME,
					oldSchemaFileName, schemaFileName));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getExchangeMode()
	 * @generated
	 */
	@Override
	public DataExchangeMode getExchangeMode() {
		return exchangeMode;
	}

	/**
	 * @param newExchangeMode
	 * @param msgs
	 * @return a chain of notifications
	 * @generated
	 */
	public NotificationChain basicSetExchangeMode(DataExchangeMode newExchangeMode, NotificationChain msgs) {
		final DataExchangeMode oldExchangeMode = exchangeMode;
		exchangeMode = newExchangeMode;

		if (eNotificationRequired()) {
			final var notification = new ENotificationImpl(this, Notification.SET, ExchangePackage.DATA_EXCHANGE_METHOD__EXCHANGE_MODE,
					oldExchangeMode, newExchangeMode);

			if (msgs == null)
				msgs = notification;
			else
				msgs.add(notification);
		}

		return msgs;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#setExchangeMode(net.codecadenza.eclipse.model.exchange.
	 * DataExchangeMode)
	 * @generated
	 */
	@Override
	public void setExchangeMode(DataExchangeMode newExchangeMode) {
		if (newExchangeMode != exchangeMode) {
			NotificationChain msgs = null;

			if (exchangeMode != null)
				msgs = ((InternalEObject) exchangeMode).eInverseRemove(this,
						EOPPOSITE_FEATURE_BASE - ExchangePackage.DATA_EXCHANGE_METHOD__EXCHANGE_MODE, null, msgs);

			if (newExchangeMode != null)
				msgs = ((InternalEObject) newExchangeMode).eInverseAdd(this,
						EOPPOSITE_FEATURE_BASE - ExchangePackage.DATA_EXCHANGE_METHOD__EXCHANGE_MODE, null, msgs);

			msgs = basicSetExchangeMode(newExchangeMode, msgs);

			if (msgs != null)
				msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.DATA_EXCHANGE_METHOD__EXCHANGE_MODE, newExchangeMode,
					newExchangeMode));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getRootElement()
	 * @generated
	 */
	@Override
	public DataExchangeElement getRootElement() {
		return rootElement;
	}

	/**
	 * @param newRootElement
	 * @param msgs
	 * @return a chain of notifications
	 * @generated
	 */
	public NotificationChain basicSetRootElement(DataExchangeElement newRootElement, NotificationChain msgs) {
		final DataExchangeElement oldRootElement = rootElement;
		rootElement = newRootElement;

		if (eNotificationRequired()) {
			final var notification = new ENotificationImpl(this, Notification.SET, ExchangePackage.DATA_EXCHANGE_METHOD__ROOT_ELEMENT,
					oldRootElement, newRootElement);

			if (msgs == null)
				msgs = notification;
			else
				msgs.add(notification);
		}

		return msgs;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#setRootElement(net.codecadenza.eclipse.model.exchange.
	 * DataExchangeElement)
	 * @generated
	 */
	@Override
	public void setRootElement(DataExchangeElement newRootElement) {
		if (newRootElement != rootElement) {
			NotificationChain msgs = null;

			if (rootElement != null)
				msgs = ((InternalEObject) rootElement).eInverseRemove(this, ExchangePackage.DATA_EXCHANGE_ELEMENT__DATA_EXCHANGE_METHOD,
						DataExchangeElement.class, msgs);

			if (newRootElement != null)
				msgs = ((InternalEObject) newRootElement).eInverseAdd(this, ExchangePackage.DATA_EXCHANGE_ELEMENT__DATA_EXCHANGE_METHOD,
						DataExchangeElement.class, msgs);

			msgs = basicSetRootElement(newRootElement, msgs);

			if (msgs != null)
				msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.DATA_EXCHANGE_METHOD__ROOT_ELEMENT, newRootElement,
					newRootElement));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getContentType()
	 * @generated
	 */
	@Override
	public ContentTypeEnumeration getContentType() {
		return contentType;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#setContentType(net.codecadenza.eclipse.model.exchange.
	 * ContentTypeEnumeration)
	 * @generated
	 */
	@Override
	public void setContentType(ContentTypeEnumeration newContentType) {
		final ContentTypeEnumeration oldContentType = contentType;
		contentType = newContentType == null ? CONTENT_TYPE_EDEFAULT : newContentType;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.DATA_EXCHANGE_METHOD__CONTENT_TYPE, oldContentType,
					contentType));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getMethodType()
	 * @generated
	 */
	@Override
	public DataExchangeMethodTypeEnumeration getMethodType() {
		return methodType;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#
	 * setMethodType(net.codecadenza.eclipse.model.exchange.DataExchangeMethodTypeEnumeration)
	 * @generated
	 */
	@Override
	public void setMethodType(DataExchangeMethodTypeEnumeration newMethodType) {
		final DataExchangeMethodTypeEnumeration oldMethodType = methodType;
		methodType = newMethodType == null ? METHOD_TYPE_EDEFAULT : newMethodType;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.DATA_EXCHANGE_METHOD__METHOD_TYPE, oldMethodType,
					methodType));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getParser()
	 * @generated
	 */
	@Override
	public ParserImplementationEnumeration getParser() {
		return parser;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#setParser(net.codecadenza.eclipse.model.exchange.
	 * ParserImplementationEnumeration)
	 * @generated
	 */
	@Override
	public void setParser(ParserImplementationEnumeration newParser) {
		final ParserImplementationEnumeration oldParser = parser;
		parser = newParser == null ? PARSER_EDEFAULT : newParser;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.DATA_EXCHANGE_METHOD__PARSER, oldParser, parser));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#isPerformValidation()
	 * @generated
	 */
	@Override
	public boolean isPerformValidation() {
		return performValidation;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#setPerformValidation(boolean)
	 * @generated
	 */
	@Override
	public void setPerformValidation(boolean newPerformValidation) {
		final boolean oldPerformValidation = performValidation;
		performValidation = newPerformValidation;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.DATA_EXCHANGE_METHOD__PERFORM_VALIDATION,
					oldPerformValidation, performValidation));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getCharset()
	 * @generated
	 */
	@Override
	public String getCharset() {
		return charset;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#setCharset(java.lang.String)
	 * @generated
	 */
	@Override
	public void setCharset(String newCharset) {
		final String oldCharset = charset;
		charset = newCharset;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.DATA_EXCHANGE_METHOD__CHARSET, oldCharset, charset));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#isFormatOutput()
	 * @generated
	 */
	@Override
	public boolean isFormatOutput() {
		return formatOutput;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#setFormatOutput(boolean)
	 * @generated
	 */
	@Override
	public void setFormatOutput(boolean newFormatOutput) {
		final boolean oldFormatOutput = formatOutput;
		formatOutput = newFormatOutput;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExchangePackage.DATA_EXCHANGE_METHOD__FORMAT_OUTPUT, oldFormatOutput,
					formatOutput));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.impl.ServiceMethodImpl#eInverseAdd(org.eclipse.emf.ecore.InternalEObject, int,
	 * org.eclipse.emf.common.notify.NotificationChain)
	 * @generated
	 */
	@Override
	public NotificationChain eInverseAdd(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case ExchangePackage.DATA_EXCHANGE_METHOD__ROOT_ELEMENT:
				if (rootElement != null)
					msgs = ((InternalEObject) rootElement).eInverseRemove(this,
							EOPPOSITE_FEATURE_BASE - ExchangePackage.DATA_EXCHANGE_METHOD__ROOT_ELEMENT, null, msgs);

				return basicSetRootElement((DataExchangeElement) otherEnd, msgs);
			case ExchangePackage.DATA_EXCHANGE_METHOD__DATA_EXCHANGE_SERVICE_BEAN:
				if (eInternalContainer() != null)
					msgs = eBasicRemoveFromContainer(msgs);

				return basicSetDataExchangeServiceBean((DataExchangeServiceBean) otherEnd, msgs);
		}

		return super.eInverseAdd(otherEnd, featureID, msgs);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.impl.ServiceMethodImpl#eInverseRemove(org.eclipse.emf.ecore.InternalEObject, int,
	 * org.eclipse.emf.common.notify.NotificationChain)
	 * @generated
	 */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case ExchangePackage.DATA_EXCHANGE_METHOD__EXCHANGE_MODE:
				return basicSetExchangeMode(null, msgs);
			case ExchangePackage.DATA_EXCHANGE_METHOD__ROOT_ELEMENT:
				return basicSetRootElement(null, msgs);
			case ExchangePackage.DATA_EXCHANGE_METHOD__DATA_EXCHANGE_SERVICE_BEAN:
				return basicSetDataExchangeServiceBean(null, msgs);
			case ExchangePackage.DATA_EXCHANGE_METHOD__ASSOCIATION_CONTROLLERS:
				return ((InternalEList<?>) getAssociationControllers()).basicRemove(otherEnd, msgs);
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
			case ExchangePackage.DATA_EXCHANGE_METHOD__DATA_EXCHANGE_SERVICE_BEAN:
				return eInternalContainer().eInverseRemove(this, ExchangePackage.DATA_EXCHANGE_SERVICE_BEAN__DATA_EXCHANGE_METHODS,
						DataExchangeServiceBean.class, msgs);
		}

		return super.eBasicRemoveFromContainerFeature(msgs);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.impl.ServiceMethodImpl#eGet(int, boolean, boolean)
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case ExchangePackage.DATA_EXCHANGE_METHOD__EXCHANGE_MODE:
				return getExchangeMode();
			case ExchangePackage.DATA_EXCHANGE_METHOD__ROOT_ELEMENT:
				return getRootElement();
			case ExchangePackage.DATA_EXCHANGE_METHOD__CONTENT_TYPE:
				return getContentType();
			case ExchangePackage.DATA_EXCHANGE_METHOD__METHOD_TYPE:
				return getMethodType();
			case ExchangePackage.DATA_EXCHANGE_METHOD__PARSER:
				return getParser();
			case ExchangePackage.DATA_EXCHANGE_METHOD__PERFORM_VALIDATION:
				return isPerformValidation();
			case ExchangePackage.DATA_EXCHANGE_METHOD__CHARSET:
				return getCharset();
			case ExchangePackage.DATA_EXCHANGE_METHOD__FORMAT_OUTPUT:
				return isFormatOutput();
			case ExchangePackage.DATA_EXCHANGE_METHOD__DATA_EXCHANGE_SERVICE_BEAN:
				return getDataExchangeServiceBean();
			case ExchangePackage.DATA_EXCHANGE_METHOD__PROCESS_SINGLE_OBJECT:
				return isProcessSingleObject();
			case ExchangePackage.DATA_EXCHANGE_METHOD__JOINED_IMPORT_METHOD:
				if (resolve)
					return getJoinedImportMethod();

				return basicGetJoinedImportMethod();
			case ExchangePackage.DATA_EXCHANGE_METHOD__QUOTE_CHARACTER:
				return getQuoteCharacter();
			case ExchangePackage.DATA_EXCHANGE_METHOD__RECORD_SEPARATOR:
				return getRecordSeparator();
			case ExchangePackage.DATA_EXCHANGE_METHOD__COMMENT_CHARACTER:
				return getCommentCharacter();
			case ExchangePackage.DATA_EXCHANGE_METHOD__DELIMITER:
				return getDelimiter();
			case ExchangePackage.DATA_EXCHANGE_METHOD__DEFAULT_DATE_FORMAT:
				return getDefaultDateFormat();
			case ExchangePackage.DATA_EXCHANGE_METHOD__DEFAULT_DATE_TIME_FORMAT:
				return getDefaultDateTimeFormat();
			case ExchangePackage.DATA_EXCHANGE_METHOD__DEFAULT_NUMBER_FORMAT:
				return getDefaultNumberFormat();
			case ExchangePackage.DATA_EXCHANGE_METHOD__ASSOCIATION_CONTROLLERS:
				return getAssociationControllers();
			case ExchangePackage.DATA_EXCHANGE_METHOD__SCHEMA_FILE_NAME:
				return getSchemaFileName();
		}

		return super.eGet(featureID, resolve, coreType);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.impl.ServiceMethodImpl#eSet(int, java.lang.Object)
	 * @generated
	 */
	@SuppressWarnings("unchecked")
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case ExchangePackage.DATA_EXCHANGE_METHOD__EXCHANGE_MODE:
				setExchangeMode((DataExchangeMode) newValue);
				return;
			case ExchangePackage.DATA_EXCHANGE_METHOD__ROOT_ELEMENT:
				setRootElement((DataExchangeElement) newValue);
				return;
			case ExchangePackage.DATA_EXCHANGE_METHOD__CONTENT_TYPE:
				setContentType((ContentTypeEnumeration) newValue);
				return;
			case ExchangePackage.DATA_EXCHANGE_METHOD__METHOD_TYPE:
				setMethodType((DataExchangeMethodTypeEnumeration) newValue);
				return;
			case ExchangePackage.DATA_EXCHANGE_METHOD__PARSER:
				setParser((ParserImplementationEnumeration) newValue);
				return;
			case ExchangePackage.DATA_EXCHANGE_METHOD__PERFORM_VALIDATION:
				setPerformValidation((Boolean) newValue);
				return;
			case ExchangePackage.DATA_EXCHANGE_METHOD__CHARSET:
				setCharset((String) newValue);
				return;
			case ExchangePackage.DATA_EXCHANGE_METHOD__FORMAT_OUTPUT:
				setFormatOutput((Boolean) newValue);
				return;
			case ExchangePackage.DATA_EXCHANGE_METHOD__DATA_EXCHANGE_SERVICE_BEAN:
				setDataExchangeServiceBean((DataExchangeServiceBean) newValue);
				return;
			case ExchangePackage.DATA_EXCHANGE_METHOD__PROCESS_SINGLE_OBJECT:
				setProcessSingleObject((Boolean) newValue);
				return;
			case ExchangePackage.DATA_EXCHANGE_METHOD__JOINED_IMPORT_METHOD:
				setJoinedImportMethod((DataExchangeMethod) newValue);
				return;
			case ExchangePackage.DATA_EXCHANGE_METHOD__QUOTE_CHARACTER:
				setQuoteCharacter((Character) newValue);
				return;
			case ExchangePackage.DATA_EXCHANGE_METHOD__RECORD_SEPARATOR:
				setRecordSeparator((String) newValue);
				return;
			case ExchangePackage.DATA_EXCHANGE_METHOD__COMMENT_CHARACTER:
				setCommentCharacter((Character) newValue);
				return;
			case ExchangePackage.DATA_EXCHANGE_METHOD__DELIMITER:
				setDelimiter((Character) newValue);
				return;
			case ExchangePackage.DATA_EXCHANGE_METHOD__DEFAULT_DATE_FORMAT:
				setDefaultDateFormat((String) newValue);
				return;
			case ExchangePackage.DATA_EXCHANGE_METHOD__DEFAULT_DATE_TIME_FORMAT:
				setDefaultDateTimeFormat((String) newValue);
				return;
			case ExchangePackage.DATA_EXCHANGE_METHOD__DEFAULT_NUMBER_FORMAT:
				setDefaultNumberFormat((String) newValue);
				return;
			case ExchangePackage.DATA_EXCHANGE_METHOD__ASSOCIATION_CONTROLLERS:
				getAssociationControllers().clear();
				getAssociationControllers().addAll((Collection<? extends AssociationController>) newValue);
				return;
			case ExchangePackage.DATA_EXCHANGE_METHOD__SCHEMA_FILE_NAME:
				setSchemaFileName((String) newValue);
				return;
		}

		super.eSet(featureID, newValue);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.impl.ServiceMethodImpl#eUnset(int)
	 * @generated
	 */
	@Override
	public void eUnset(int featureID) {
		switch (featureID) {
			case ExchangePackage.DATA_EXCHANGE_METHOD__EXCHANGE_MODE:
				setExchangeMode((DataExchangeMode) null);
				return;
			case ExchangePackage.DATA_EXCHANGE_METHOD__ROOT_ELEMENT:
				setRootElement((DataExchangeElement) null);
				return;
			case ExchangePackage.DATA_EXCHANGE_METHOD__CONTENT_TYPE:
				setContentType(CONTENT_TYPE_EDEFAULT);
				return;
			case ExchangePackage.DATA_EXCHANGE_METHOD__METHOD_TYPE:
				setMethodType(METHOD_TYPE_EDEFAULT);
				return;
			case ExchangePackage.DATA_EXCHANGE_METHOD__PARSER:
				setParser(PARSER_EDEFAULT);
				return;
			case ExchangePackage.DATA_EXCHANGE_METHOD__PERFORM_VALIDATION:
				setPerformValidation(PERFORM_VALIDATION_EDEFAULT);
				return;
			case ExchangePackage.DATA_EXCHANGE_METHOD__CHARSET:
				setCharset(CHARSET_EDEFAULT);
				return;
			case ExchangePackage.DATA_EXCHANGE_METHOD__FORMAT_OUTPUT:
				setFormatOutput(FORMAT_OUTPUT_EDEFAULT);
				return;
			case ExchangePackage.DATA_EXCHANGE_METHOD__DATA_EXCHANGE_SERVICE_BEAN:
				setDataExchangeServiceBean((DataExchangeServiceBean) null);
				return;
			case ExchangePackage.DATA_EXCHANGE_METHOD__PROCESS_SINGLE_OBJECT:
				setProcessSingleObject(PROCESS_SINGLE_OBJECT_EDEFAULT);
				return;
			case ExchangePackage.DATA_EXCHANGE_METHOD__JOINED_IMPORT_METHOD:
				setJoinedImportMethod((DataExchangeMethod) null);
				return;
			case ExchangePackage.DATA_EXCHANGE_METHOD__QUOTE_CHARACTER:
				setQuoteCharacter(QUOTE_CHARACTER_EDEFAULT);
				return;
			case ExchangePackage.DATA_EXCHANGE_METHOD__RECORD_SEPARATOR:
				setRecordSeparator(RECORD_SEPARATOR_EDEFAULT);
				return;
			case ExchangePackage.DATA_EXCHANGE_METHOD__COMMENT_CHARACTER:
				setCommentCharacter(COMMENT_CHARACTER_EDEFAULT);
				return;
			case ExchangePackage.DATA_EXCHANGE_METHOD__DELIMITER:
				setDelimiter(DELIMITER_EDEFAULT);
				return;
			case ExchangePackage.DATA_EXCHANGE_METHOD__DEFAULT_DATE_FORMAT:
				setDefaultDateFormat(DEFAULT_DATE_FORMAT_EDEFAULT);
				return;
			case ExchangePackage.DATA_EXCHANGE_METHOD__DEFAULT_DATE_TIME_FORMAT:
				setDefaultDateTimeFormat(DEFAULT_DATE_TIME_FORMAT_EDEFAULT);
				return;
			case ExchangePackage.DATA_EXCHANGE_METHOD__DEFAULT_NUMBER_FORMAT:
				setDefaultNumberFormat(DEFAULT_NUMBER_FORMAT_EDEFAULT);
				return;
			case ExchangePackage.DATA_EXCHANGE_METHOD__ASSOCIATION_CONTROLLERS:
				getAssociationControllers().clear();
				return;
			case ExchangePackage.DATA_EXCHANGE_METHOD__SCHEMA_FILE_NAME:
				setSchemaFileName(SCHEMA_FILE_NAME_EDEFAULT);
				return;
		}

		super.eUnset(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.impl.ServiceMethodImpl#eIsSet(int)
	 * @generated
	 */
	@Override
	public boolean eIsSet(int featureID) {
		switch (featureID) {
			case ExchangePackage.DATA_EXCHANGE_METHOD__EXCHANGE_MODE:
				return exchangeMode != null;
			case ExchangePackage.DATA_EXCHANGE_METHOD__ROOT_ELEMENT:
				return rootElement != null;
			case ExchangePackage.DATA_EXCHANGE_METHOD__CONTENT_TYPE:
				return contentType != CONTENT_TYPE_EDEFAULT;
			case ExchangePackage.DATA_EXCHANGE_METHOD__METHOD_TYPE:
				return methodType != METHOD_TYPE_EDEFAULT;
			case ExchangePackage.DATA_EXCHANGE_METHOD__PARSER:
				return parser != PARSER_EDEFAULT;
			case ExchangePackage.DATA_EXCHANGE_METHOD__PERFORM_VALIDATION:
				return performValidation != PERFORM_VALIDATION_EDEFAULT;
			case ExchangePackage.DATA_EXCHANGE_METHOD__CHARSET:
				return charset != null;
			case ExchangePackage.DATA_EXCHANGE_METHOD__FORMAT_OUTPUT:
				return formatOutput != FORMAT_OUTPUT_EDEFAULT;
			case ExchangePackage.DATA_EXCHANGE_METHOD__DATA_EXCHANGE_SERVICE_BEAN:
				return getDataExchangeServiceBean() != null;
			case ExchangePackage.DATA_EXCHANGE_METHOD__PROCESS_SINGLE_OBJECT:
				return processSingleObject != PROCESS_SINGLE_OBJECT_EDEFAULT;
			case ExchangePackage.DATA_EXCHANGE_METHOD__JOINED_IMPORT_METHOD:
				return joinedImportMethod != null;
			case ExchangePackage.DATA_EXCHANGE_METHOD__QUOTE_CHARACTER:
				return quoteCharacter != QUOTE_CHARACTER_EDEFAULT;
			case ExchangePackage.DATA_EXCHANGE_METHOD__RECORD_SEPARATOR:
				return recordSeparator != null;
			case ExchangePackage.DATA_EXCHANGE_METHOD__COMMENT_CHARACTER:
				return commentCharacter != COMMENT_CHARACTER_EDEFAULT;
			case ExchangePackage.DATA_EXCHANGE_METHOD__DELIMITER:
				return delimiter != DELIMITER_EDEFAULT;
			case ExchangePackage.DATA_EXCHANGE_METHOD__DEFAULT_DATE_FORMAT:
				return defaultDateFormat != null;
			case ExchangePackage.DATA_EXCHANGE_METHOD__DEFAULT_DATE_TIME_FORMAT:
				return defaultDateTimeFormat != null;
			case ExchangePackage.DATA_EXCHANGE_METHOD__DEFAULT_NUMBER_FORMAT:
				return defaultNumberFormat != null;
			case ExchangePackage.DATA_EXCHANGE_METHOD__ASSOCIATION_CONTROLLERS:
				return associationControllers != null && !associationControllers.isEmpty();
			case ExchangePackage.DATA_EXCHANGE_METHOD__SCHEMA_FILE_NAME:
				return schemaFileName != null;
		}

		return super.eIsSet(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.impl.ServiceMethodImpl#toString()
	 * @generated
	 */
	@Override
	public String toString() {
		if (eIsProxy())
			return super.toString();

		final var result = new StringBuilder(super.toString());
		result.append(" (contentType: ");
		result.append(contentType);
		result.append(", methodType: ");
		result.append(methodType);
		result.append(", parser: ");
		result.append(parser);
		result.append(", performValidation: ");
		result.append(performValidation);
		result.append(", charset: ");
		result.append(charset);
		result.append(", formatOutput: ");
		result.append(formatOutput);
		result.append(", processSingleObject: ");
		result.append(processSingleObject);
		result.append(", quoteCharacter: ");
		result.append(quoteCharacter);
		result.append(", recordSeparator: ");
		result.append(recordSeparator);
		result.append(", commentCharacter: ");
		result.append(commentCharacter);
		result.append(", delimiter: ");
		result.append(delimiter);
		result.append(", defaultDateFormat: ");
		result.append(defaultDateFormat);
		result.append(", defaultDateTimeFormat: ");
		result.append(defaultDateTimeFormat);
		result.append(", defaultNumberFormat: ");
		result.append(defaultNumberFormat);
		result.append(", schemaFileName: ");
		result.append(schemaFileName);
		result.append(')');

		return result.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getRootElement(boolean)
	 * @generated not
	 */
	@Override
	public DataExchangeElement getRootElement(boolean searchInJoinedMethod) {
		if (!searchInJoinedMethod)
			return getRootElement();

		if (joinedImportMethod != null)
			return joinedImportMethod.getRootElement();

		return getRootElement();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#returnsPath()
	 * @generated not
	 */
	@Override
	public boolean returnsPath() {
		if (getMethodType() == DataExchangeMethodTypeEnumeration.IMPORT)
			return false;

		return getExchangeMode() instanceof FileExchangeMode && getReturnType().isString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#hasPathParameter()
	 * @generated not
	 */
	@Override
	public boolean hasPathParameter() {
		if (getMethodType() == DataExchangeMethodTypeEnumeration.EXPORT)
			return false;

		if (getExchangeMode() instanceof FileExchangeMode)
			for (final MethodParameter p : getMethodParameters())
				if (p.getName().equals(EXCHANGE_PATH_PARAM) && p.getType().isString())
					return true;

		return false;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getSingleObjectFilterParam()
	 * @generated not
	 */
	@Override
	public FilterMethodParameter getSingleObjectFilterParam() {
		if (getMethodType() == DataExchangeMethodTypeEnumeration.IMPORT)
			return null;

		if (!processSingleObject)
			return null;

		for (final MethodParameter exchangeParam : getMethodParameters()) {
			if (exchangeParam instanceof final FilterMethodParameter filterParam && filterParam.getAssociation() == null
					&& filterParam.getDomainAttribute().equals(getDataExchangeServiceBean().getDomainObject().getPKAttribute()))
				return filterParam;
		}

		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#returnsContent()
	 * @generated not
	 */
	@Override
	public boolean returnsContent() {
		if (getMethodType() == DataExchangeMethodTypeEnumeration.IMPORT)
			return false;

		if (returnsPath())
			return false;

		return getReturnType().isString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getDefaultFileExtension()
	 * @generated not
	 */
	@Override
	public String getDefaultFileExtension() {
		return getContentType().getDefaultFileExtension();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getStandardCharset()
	 * @generated not
	 */
	@Override
	public String getStandardCharset() {
		if (getCharset() == null || getCharset().isEmpty() || StandardCharsets.UTF_8.displayName().equals(getCharset()))
			return "StandardCharsets.UTF_8";
		else if (StandardCharsets.UTF_16.displayName().equals(getCharset()))
			return "StandardCharsets.UTF_16";
		else if (StandardCharsets.UTF_16BE.displayName().equals(getCharset()))
			return "StandardCharsets.UTF_16BE";
		else if (StandardCharsets.UTF_16LE.displayName().equals(getCharset()))
			return "StandardCharsets.UTF_16LE";
		else if (StandardCharsets.ISO_8859_1.displayName().equals(getCharset()))
			return "StandardCharsets.ISO_8859_1";
		else if (StandardCharsets.US_ASCII.displayName().equals(getCharset()))
			return "StandardCharsets.US_ASCII";

		return "\"" + getCharset() + "\"";
	}

}
